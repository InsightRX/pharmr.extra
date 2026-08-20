#' NWPRI uncertainty engine: run one regimen's draws in chunked NONMEM jobs
#'
#' The `uncertainty_engine = "nwpri"` route of [run_sim()]. NONMEM draws the
#' parameter vectors itself, one per `$SIMULATION` subproblem, from the
#' `$PRIOR NWPRI` record [add_nwpri_prior()] put into the model. That means a
#' single compile for the whole set of draws instead of one per draw, but it
#' also means NONMEM runs the subproblems sequentially in a single process:
#' MPI/`PARAFILE` parallelises estimation and covariance steps only, and a
#' simulation-only model has neither.
#'
#' So the parallelism is at the process level: split the subproblems over
#' `n_chunks` NONMEM jobs, each with its own run folder and seed, and
#' concatenate the tables. Each chunk differs from the others only in two
#' numbers in the `$SIMULATION` record, so the workers never need Pharmpy —
#' they get a folder and a control stream, and do [call_nmfe()] plus a table
#' read.
#'
#' @param sim_code the finished simulation control stream for this regimen:
#' `$PRIOR` records in place, `$DATA` pointing at the dataset, `$TABLE` records
#' set up. Its `$SIMULATION` record is rewritten per chunk.
#' @param n_uncertainty total number of parameter draws to produce.
#' @param n_chunks number of NONMEM jobs to split them over.
#' @param seed base random seed; chunk seeds are derived from it.
#' @param folder run folder for this regimen. Chunk `k` runs in
#' `<folder>/uncertainty_chunk_<k>`.
#' @param output_file name of the simulation output table.
#' @param nmfe path to the nmfe script, resolved by the caller (the worker
#' processes must not touch Pharmpy).
#' @param n_cores number of worker processes to spread the chunks over.
#' @param force overwrite existing chunk run folders?
#' @param verbose verbose output?
#'
#' @returns a data.frame of the concatenated subproblem tables with a 1-based
#' `.uncertainty` column running over all chunks, carrying
#' `n_uncertainty_requested` / `n_uncertainty_kept` attributes.
#' @noRd
run_nwpri_regimen <- function(
    sim_code,
    n_uncertainty,
    n_chunks,
    seed,
    folder,
    output_file,
    nmfe,
    n_cores = 1L,
    force = FALSE,
    verbose = TRUE
) {
  sizes   <- nwpri_chunk_sizes(n_uncertainty, n_chunks)
  seeds   <- nwpri_chunk_seeds(seed, length(sizes))
  offsets <- cumsum(c(0L, utils::head(sizes, -1L)))

  ## Folders are created and control streams written here rather than in the
  ## workers: `create_run_folder()` carries the `force` semantics used
  ## everywhere else, and preparing everything up front fails fast on a stale
  ## run folder or an unwritable path instead of once per chunk. It also leaves
  ## the workers with nothing to do but run NONMEM and read the table back.
  specs <- lapply(seq_along(sizes), function(k) {
    chunk_folder <- create_run_folder(
      id = paste0("uncertainty_chunk_", k),
      path = folder,
      force = force,
      verbose = FALSE
    )
    chunk_folder <- normalizePath(chunk_folder, mustWork = TRUE)
    code <- set_simulation_record(
      code = sim_code, seed = seeds[k], n = sizes[k], true_prior = TRUE
    )
    writeLines(code, file.path(chunk_folder, "run.mod"))
    list(
      index  = k,
      folder = chunk_folder,
      size   = sizes[k],
      offset = offsets[k]
    )
  })

  if(verbose) {
    cli::cli_alert_info(
      "Running {n_uncertainty} NWPRI draw{?s} in {length(specs)} NONMEM \\
       job{?s} on {n_cores} core{?s}"
    )
  }
  chunks <- parallel_lapply(
    specs,
    make_nwpri_chunk_fn(nmfe = nmfe, output_file = output_file),
    n_cores = n_cores
  )

  collect_nwpri_chunks(chunks, specs, n_uncertainty)
}

#' Build the worker function that runs one NWPRI chunk
#'
#' A factory rather than an inline closure, for the same reason as
#' `make_nlmixr_replicate_fn()`: the closure travels to the worker together
#' with its enclosing environment, and `run_sim()`'s frame holds Pharmpy
#' (Python) objects that must not be serialised. This frame holds only strings.
#'
#' @param nmfe path to the nmfe script.
#' @param output_file name of the simulation output table.
#'
#' @returns a function taking one chunk spec and returning its
#' [run_captured()] envelope.
#' @noRd
make_nwpri_chunk_fn <- function(nmfe, output_file) {
  force(nmfe)
  force(output_file)
  function(spec) {
    run_captured(spec$index, function() {
      call_nmfe(
        model_file  = "run.mod",
        output_file = "run.lst",
        path        = spec$folder,
        nmfe        = nmfe,
        verbose     = FALSE
      )
      table_path <- file.path(spec$folder, output_file)
      if(!file.exists(table_path)) {
        ## Neither pharmpy nor nmfe raise when a simulation writes no table, so
        ## surface the .lst error here instead of returning an empty chunk.
        abort_on_failed_sim(
          regimen_label = paste0("NWPRI chunk ", spec$index),
          fit_folder = spec$folder
        )
      }
      read_table_nm(file = table_path, subproblems = TRUE)
    })
  }
}

#' Renumber and concatenate the chunk tables of an NWPRI run
#'
#' The subproblem counter each chunk returns is chunk-local, so it is shifted
#' by the chunk's offset to give a `.uncertainty` index that runs 1..n over the
#' whole set of draws. A chunk that failed is dropped with a warning rather
#' than taking the run down with it — but note this is a coarser granularity
#' than the `"replicates"` engine, where a failure costs one draw rather than
#' `n / n_chunks` of them.
#'
#' @param chunks list of [run_captured()] envelopes, in spec order.
#' @param specs the chunk specs the envelopes came from.
#' @param n_uncertainty number of draws requested.
#'
#' @returns a data.frame with `.uncertainty`, and `n_uncertainty_requested` /
#' `n_uncertainty_kept` attributes.
#' @noRd
collect_nwpri_chunks <- function(chunks, specs, n_uncertainty) {
  out <- lapply(seq_along(chunks), function(k) {
    chunk <- chunks[[k]]
    spec  <- specs[[k]]
    emit_replicate_warnings(spec$index, chunk$warnings, label = "Uncertainty chunk")
    res <- chunk$result
    if(inherits(res, "condition")) {
      cli::cli_warn(
        "Uncertainty chunk {spec$index} ({spec$size} draw{?s}) failed \\
         ({conditionMessage(res)}); omitted."
      )
      return(NULL)
    }
    if(is.null(res) || nrow(res) == 0) {
      cli::cli_warn(
        "Uncertainty chunk {spec$index} ({spec$size} draw{?s}) produced no \\
         simulation output; omitted."
      )
      return(NULL)
    }
    n_sub <- length(unique(res[[".subproblem"]]))
    if(n_sub != spec$size) {
      ## A short chunk would silently shift nothing (the offsets are fixed), but
      ## it does mean fewer draws than asked for, so say so.
      cli::cli_warn(
        "Uncertainty chunk {spec$index} returned {n_sub} subproblem{?s} of the \\
         {spec$size} requested."
      )
    }
    res[[".uncertainty"]] <- as.integer(spec$offset + res[[".subproblem"]])
    res[[".subproblem"]] <- NULL
    res
  }) |>
    dplyr::bind_rows()

  if(nrow(out) == 0) {
    cli::cli_abort(c(
      "All {length(chunks)} NWPRI chunk{?s} failed; no simulation output.",
      i = "See the warnings above for the individual failures."
    ))
  }

  n_kept <- length(unique(out[[".uncertainty"]]))
  attr(out, "n_uncertainty_requested") <- n_uncertainty
  attr(out, "n_uncertainty_kept") <- n_kept
  if(n_kept < n_uncertainty) {
    cli::cli_warn(c(
      "Only {n_kept} of {n_uncertainty} NWPRI draw{?s} produced output.",
      i = "A failed chunk costs {.field n_uncertainty / n_chunks} draws at \\
           once, so intervals computed over {.field .uncertainty} may be \\
           based on many fewer draws than requested.",
      i = "The counts are on the result as the {.field n_uncertainty_kept} and \\
           {.field n_uncertainty_requested} attributes."
    ))
  }
  out
}

#' Split `n` subproblems over `n_chunks` NONMEM jobs
#'
#' As even as possible, remainder spread over the leading chunks. Never more
#' chunks than draws: an empty `$SIMULATION` record is not a thing.
#'
#' @param n total number of subproblems.
#' @param n_chunks requested number of chunks.
#'
#' @returns an integer vector of chunk sizes summing to `n`.
#' @noRd
nwpri_chunk_sizes <- function(n, n_chunks) {
  n <- as.integer(n)
  n_chunks <- as.integer(n_chunks)
  if(is.na(n) || n < 1L) {
    cli::cli_abort("Number of NWPRI draws must be a positive integer.")
  }
  if(is.na(n_chunks) || n_chunks < 1L) {
    cli::cli_abort("`n_chunks` must be a positive integer.")
  }
  k <- min(n, n_chunks)
  base <- n %/% k
  rem  <- n %% k
  as.integer(base + c(rep(1L, rem), rep(0L, k - rem)))
}

#' Derive per-chunk NONMEM seeds from a base seed
#'
#' Separate seeds are not formally independent streams; spacing them widely
#' makes overlapping draw sequences unlikely in practice. Kept inside NONMEM's
#' seed range, which tops out just below `2^31`.
#'
#' Note that the draws therefore depend on how the subproblems were chunked:
#' unlike the `"replicates"` engine, an NWPRI run is only reproducible for a
#' fixed `n_chunks`.
#'
#' @param seed base seed.
#' @param n_chunks number of chunks.
#' @param spacing gap between consecutive chunk seeds.
#'
#' @returns an integer vector of `n_chunks` distinct seeds.
#' @noRd
nwpri_chunk_seeds <- function(seed, n_chunks, spacing = 1000003L) {
  seed <- suppressWarnings(as.numeric(seed))
  if(length(seed) != 1 || is.na(seed) || seed < 0 || seed != round(seed)) {
    cli::cli_abort("`seed` must be a non-negative integer for an NWPRI run.")
  }
  max_seed <- 2147483646
  seeds <- (seed + (seq_len(n_chunks) - 1) * spacing) %% max_seed
  ## NONMEM wants a positive seed; 0 is what the modulo can land on.
  seeds[seeds == 0] <- max_seed
  if(anyDuplicated(seeds) > 0) {
    cli::cli_abort(c(
      "Could not derive {n_chunks} distinct NONMEM seeds from `seed = {seed}`.",
      i = "Use fewer chunks, or a different seed."
    ))
  }
  as.integer(seeds)
}

#' Resolve the number of chunks an NWPRI run is split over
#'
#' Defaults to `n_cores`, which is the throughput-optimal choice but makes the
#' draws depend on the machine: the parameter vectors come out of NONMEM's own
#' RNG, so which ones you get depends on how the subproblems were chunked. Set
#' `n_chunks` explicitly to keep a run reproducible across machines.
#'
#' @param n_chunks requested number of chunks, or `NULL` for `n_cores`.
#' @param n_cores resolved number of worker processes.
#'
#' @returns a positive integer.
#' @noRd
resolve_n_chunks <- function(n_chunks, n_cores) {
  if(is.null(n_chunks)) return(as.integer(n_cores))
  n <- suppressWarnings(as.numeric(n_chunks))
  if(length(n) != 1 || is.na(n) || n < 1 || n != round(n) ||
     n > .Machine$integer.max) {
    cli::cli_abort("`n_chunks` must be a positive integer (<= {(.Machine$integer.max)}) or NULL.")
  }
  as.integer(n)
}

#' Run one regimen's NWPRI draws and return its output tables
#'
#' The `uncertainty_engine = "nwpri"` counterpart of the [run_nlme()] call the
#' regimen loop of [run_sim()] otherwise makes. Same contract: a named list of
#' output tables for this regimen, with the simulation table first.
#'
#' Unlike the `"replicates"` engine there is no [run_nlme()] here at all. The
#' control stream is already finished at this point (simulation-only, tables
#' set up, `$PRIOR` records in place), so the run folder is prepared directly
#' and handed to [run_nwpri_regimen()] to chunk.
#'
#' @param sim_model the Pharmpy simulation model for this regimen.
#' @param sim_data_regimen this regimen's simulation dataset.
#' @param reg_label label of the regimen, used in error messages.
#' @param id run id for this regimen (e.g. `sim_x/regimen_1`).
#' @param path folder the run id is created under.
#' @inheritParams run_nwpri_regimen
#' @param update_table were the `$TABLE` records rebuilt by [run_sim()]?
#' @param add_pk_variables add derived PK variables to the output table?
#'
#' @returns named list of data.frames, one per output table.
#' @noRd
run_nwpri_regimen_tables <- function(
    sim_model,
    sim_data_regimen,
    reg_label,
    id,
    path,
    n_uncertainty,
    n_chunks,
    seed,
    nmfe,
    update_table = TRUE,
    add_pk_variables = FALSE,
    n_cores = 1L,
    force = TRUE,
    verbose = TRUE
) {
  reg_folder <- create_run_folder(
    id = id, path = path, force = force, verbose = FALSE
  )
  reg_folder <- normalizePath(reg_folder, mustWork = TRUE)

  ## One dataset for the whole regimen, shared by every chunk: `$DATA` is
  ## rewritten to an absolute path so the chunk folders below it can all read
  ## the same file.
  dataset_path <- file.path(reg_folder, "data.csv")
  write.csv(unquote_column_names(sim_data_regimen), dataset_path,
            quote = FALSE, row.names = FALSE)
  sim_code <- change_nonmem_dataset(sim_model$code, dataset_path)

  ## Which table to read back. Derived from the control stream rather than
  ## taken from `run_sim()`'s `output_file`, so `update_table = FALSE` (tables
  ## as the model declares them) works too — and it matches what the
  ## `"replicates"` engine does, which reads back whatever the model wrote.
  table_names <- get_tables_in_model_code(sim_code)
  if(length(table_names) == 0) {
    cli::cli_abort(c(
      "The simulation model for regimen {.val {reg_label}} has no $TABLE record.",
      i = "Nothing would be written for the NWPRI draws to be read back from."
    ))
  }
  table_name <- table_names[1]

  tab <- run_nwpri_regimen(
    sim_code      = sim_code,
    n_uncertainty = n_uncertainty,
    n_chunks      = n_chunks,
    seed          = seed,
    folder        = reg_folder,
    output_file   = table_name,
    nmfe          = nmfe,
    n_cores       = n_cores,
    force         = force,
    verbose       = verbose
  )

  if(update_table && add_pk_variables) {
    regimen_for_pk <- NULL
    if(all(c("EVID", "AMT") %in% names(sim_data_regimen))) {
      dose_rows <- sim_data_regimen[sim_data_regimen$EVID == 1, , drop = FALSE]
      if(nrow(dose_rows) > 0) regimen_for_pk <- list(dose = dose_rows$AMT)
    }
    ## Per draw, not over the whole table: every subproblem repeats the same
    ## IDs, so a Cmax taken over the concatenation would be the maximum over
    ## all draws rather than this draw's.
    tab <- tab |>
      dplyr::group_split(.data$.uncertainty) |>
      lapply(calc_pk_variables, regimen = regimen_for_pk) |>
      dplyr::bind_rows() |>
      dplyr::ungroup()
  }

  stats::setNames(list(tab), table_name)
}
