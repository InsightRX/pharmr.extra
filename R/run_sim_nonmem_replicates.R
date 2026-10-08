## Replicates uncertainty engine, NONMEM backend: prepare / execute split
##
## The `uncertainty_engine = "replicates"` route of run_sim() for NONMEM. Every
## replicate is an independent NONMEM run -- its own parameter draw, its own run
## folder, combined only at the end -- so the replicates can be spread over
## worker processes. What stops them going there as-is is that building one is
## Pharmpy work (`pharmr::set_initial_estimates()`, `set_simulation_clean()`,
## the $TABLE records), and a Pharmpy model object cannot cross a process
## boundary.
##
## So the work is split where Python stops being needed: the parent applies the
## draw, renders the control stream and writes it into a run folder together
## with the dataset, and the worker only calls `call_nmfe()` and reads the
## output tables back -- plain R over plain R data (a folder, two filenames and
## the table names to look for). This is the same parent-prepares /
## worker-executes split the nlmixr2 path and the NWPRI engine use. See #129.
##
## Each replicate gets its own run folder, `id/uncertainty_<r>/regimen_<i>`,
## rather than every replicate reusing `id/regimen_<i>`. Concurrent replicates
## would otherwise clobber each other's run.mod, dataset and output tables;
## sequentially they merely overwrote them, which left the per-replicate NONMEM
## artifacts of everything but the last replicate impossible to inspect after
## the fact.

#' Resolve the simulation dataset and split it into per-regimen jobs
#'
#' The dataset half of [run_sim()]'s regimen loop, lifted out so the replicate
#' path can do it once in the parent instead of once per replicate: the
#' dataset is the same for every draw.
#'
#' @param data the caller's `data`, or `NULL` to use the model's own dataset.
#' @param input_data the model's dataset, used as the column-order reference.
#' @param verbose verbose output?
#'
#' @returns a list with one element per regimen: `index` (1-based),
#' `label` (the `.regimen` value), `data` (that regimen's dataset, sorted and
#' with `.regimen` dropped) and `regimen_for_pk` (the dosing regimen
#' [calc_pk_variables()] needs, or `NULL`).
#' @param default_dose_cmt the compartment a dose without one goes into (see
#' [get_default_dose_compartment()]).
#' @noRd
resolve_sim_regimens <- function(
    data,
    input_data,
    default_dose_cmt = 1,
    verbose = TRUE
) {
  if(is.null(data)) {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- as.data.frame(input_data)
    sim_data[[".regimen"]] <- "original regimens"
  } else {
    validate_sim_data(data)
    sim_data <- data
    if(!".regimen" %in% names(sim_data)) {
      sim_data[[".regimen"]] <- "original regimens"
    }
  }

  unique_regimens <- unique(sim_data[[".regimen"]])
  lapply(seq_along(unique_regimens), function(i) {
    reg_label <- unique_regimens[i]
    reg_data <- sim_data |>
      dplyr::filter(.data$.regimen == reg_label) |>
      dplyr::select(-".regimen")
    if("EVID" %in% names(reg_data)) {
      reg_data <- reg_data |>
        dplyr::arrange(.data$ID, .data$TIME, -.data$EVID)
    } else {
      reg_data <- reg_data |>
        dplyr::arrange(.data$ID, .data$TIME)
    }
    ## Ensure column names & order match the model's dataset, which is the
    ## order NONMEM reads `$INPUT` in. Only when the two hold the same columns:
    ## a simulation dataset with a strict subset of them (a model covariate the
    ## caller's `data` leaves out, say) also satisfies `%in%`, but indexing by
    ## `names(input_data)` would then error with "undefined columns selected"
    ## rather than leave the dataset as it came in.
    if(setequal(names(reg_data), names(input_data))) {
      reg_data <- reg_data[, names(input_data)]
    }
    list(
      index          = i,
      label          = reg_label,
      data           = reg_data,
      regimen_for_pk = sim_regimen_doses(reg_data, default_cmt = default_dose_cmt)
    )
  })
}

#' The dosing regimen `calc_pk_variables()` needs, derived from a dataset
#'
#' AUC_SS is dose over CL, so the doses have to come from the simulation
#' dataset rather than from the model. Each dose carries the subject it belongs
#' to, since subjects in one regimen need not receive the same dose (a
#' weight-based regimen, say), and the compartment it goes into, since that is
#' what decides which bioavailability (`F<n>`) applies to it. Its time is
#' what assigns observations to dosing intervals for CMIN_OBS, as the
#' simulation output need not hold the dose records (nlmixr2's does not).
#'
#' Doses implied by `ADDL`/`II` are expanded into records of their own.
#'
#' @param data one regimen's simulation dataset.
#' @param default_cmt the compartment a dose without one (no, an empty or a
#' zero `CMT`) goes into.
#'
#' @returns `list(dose = , id = , time = , cmt = , default_cmt = )` with one
#' element per dose (doses implied by `ADDL`/`II` included) in the first four
#' (`id`, `time` and `cmt` are `NULL` without an `ID`, `TIME` or `CMT`
#' column), or `NULL` when the dataset has no
#' dose records. A dataset with reset records (`EVID` 3 or 4) adds `reset`,
#' a `data.frame(id = , time = )` of them.
#' @noRd
sim_regimen_doses <- function(data, default_cmt = 1) {
  if(!all(c("EVID", "AMT") %in% names(data))) return(NULL)
  dose_rows <- data[data$EVID %in% c(1, 4), , drop = FALSE]
  if(nrow(dose_rows) == 0) return(NULL)
  reset_rows <- data[data$EVID %in% c(3, 4), , drop = FALSE]
  dose_rows <- expand_addl_doses(dose_rows)
  out <- list(
    dose = dose_rows$AMT,
    id   = dose_rows[["ID"]],
    time = dose_rows[["TIME"]],
    cmt  = dose_rows[["CMT"]],
    default_cmt = default_cmt
  )
  ## Where the system is reset (a new occasion): the simulation output need
  ## not hold these records either.
  if(nrow(reset_rows) > 0 && "TIME" %in% names(reset_rows)) {
    out$reset <- data.frame(
      id = if("ID" %in% names(reset_rows)) reset_rows$ID else NA,
      time = reset_rows$TIME
    )
  }
  out
}

#' The compartment a NONMEM model doses into by default
#'
#' A dose record without a compartment (no, an empty or a zero `CMT`) goes
#' into the compartment `$MODEL` marks `DEFDOSE`, and into compartment 1 where
#' none is marked or there is no `$MODEL` (the predefined ADVANs).
#'
#' @param code NONMEM control stream.
#'
#' @returns the compartment number.
#' @noRd
get_default_dose_compartment <- function(code) {
  code <- paste(code, collapse = "\n")
  if(length(code) == 0 || is.na(code)) return(1)
  ## Comments out, then the $MODEL record: up to the next record
  code <- gsub(";[^\n]*", "", code)
  model_rec <- stringr::str_match(
    code, "(?is)\\$MODEL?\\b(.*?)(?=\\n\\s*\\$|$)"
  )[, 2]
  if(is.na(model_rec)) return(1)
  comps <- stringr::str_match_all(
    model_rec,
    "(?i)(?<![A-Za-z])COMP(?:ARTMENT)?\\s*=\\s*(\\([^)]*\\)|[^\\s(]+)"
  )[[1]][, 2]
  is_default <- grepl("(?i)\\bDEFDOS(E)?\\b", comps, perl = TRUE)
  if(!any(is_default)) return(1)
  which(is_default)[1]
}

#' Expand the doses implied by `ADDL`/`II` into dose records of their own
#'
#' A record with `ADDL = n` and `II = tau` stands for itself plus `n` more
#' doses `tau` apart, which matter both for the dosing intervals CMIN_OBS is
#' taken over and for which dose is a subject's last.
#'
#' @param dose_rows dose records (`EVID` 1 or 4) with `TIME`, and optionally
#' `ADDL` and `II`.
#'
#' @returns `dose_rows`, with an extra record for every implied dose, in
#' order of subject, occasion (see `time_segments()`) and time.
#' @noRd
expand_addl_doses <- function(dose_rows) {
  if(!all(c("TIME", "ADDL", "II") %in% names(dose_rows))) return(dose_rows)
  suppressWarnings({
    addl <- as.numeric(dose_rows$ADDL)
    ii <- as.numeric(dose_rows$II)
  })
  n_extra <- ifelse(!is.na(addl) & addl > 0 & !is.na(ii) & ii > 0,
                    floor(addl), 0)
  if(all(n_extra == 0)) return(dose_rows)
  idx <- rep(seq_len(nrow(dose_rows)), n_extra + 1)
  out <- dose_rows[idx, , drop = FALSE]
  ## Only the generated copies move: a record without ADDL keeps its time
  ## whatever its II (often missing, `.`).
  k <- sequence(n_extra + 1) - 1
  is_copy <- k > 0
  suppressWarnings(time <- as.numeric(out$TIME))
  time[is_copy] <- time[is_copy] + k[is_copy] * ii[idx][is_copy]
  out$TIME <- time
  out$ADDL <- 0
  ## Sorted by time within each subject and occasion: a subject whose time
  ## restarts (a reset, a new occasion) keeps its occasions in dataset order.
  subject <- if("ID" %in% names(out)) {
    match(out$ID, unique(out$ID))
  } else {
    rep(1L, nrow(out))
  }
  occasion <- time_segments(
    as.numeric(dose_rows$TIME), dose_rows[["ID"]],
    reset = dose_rows[["EVID"]] %in% 4
  )[idx]
  out <- out[order(subject, occasion, out$TIME), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Number the stretches of rows over which time does not go back
#'
#' A subject's time restarting (an `EVID` 3/4 reset into a new occasion, a
#' new simulation subproblem) starts a new stretch.
#'
#' @param time numeric vector, in dataset order.
#' @param id subject of each element, or `NULL` for a single subject.
#' @param reset logical, elements that start a new stretch regardless of time
#' (an `EVID` 4 dose), or `NULL`.
#'
#' @returns integer vector the length of `time`: 1 for each subject's first
#' stretch, 2 for its second, and so on.
#' @noRd
time_segments <- function(time, id = NULL, reset = NULL) {
  n <- length(time)
  if(n == 0) return(integer(0))
  id <- if(is.null(id)) rep("", n) else as.character(id)
  new_subject <- c(TRUE, id[-1] != id[-n])
  goes_back <- c(FALSE, !is.na(time[-1]) & !is.na(time[-n]) & time[-1] < time[-n])
  if(is.null(reset)) reset <- rep(FALSE, n)
  block <- cumsum(new_subject | goes_back | reset %in% TRUE)
  first <- !duplicated(block)
  segment <- stats::ave(seq_along(block[first]), id[first], FUN = seq_along)
  as.integer(segment[block])
}

#' Turn a model into a simulation-only model with the requested `$TABLE`
#'
#' The Pharmpy half of [run_sim()]'s regimen loop. Note the result does not
#' depend on the regimen — only on the model, the seed and the requested
#' output variables — so it is built once per replicate and reused for every
#' regimen, which is also why the replicate path can prepare it in the parent.
#'
#' @param model the (draw-updated) Pharmpy model.
#' @param seed simulation seed.
#' @param n_iterations number of `$SIMULATION` subproblems.
#' @param update_table rebuild the `$TABLE` records?
#' @param variables variables to output, or `NULL` for the defaults.
#' @param output_file name of the simulation output table.
#' @param verbose verbose output?
#'
#' @returns a Pharmpy model object.
#' @noRd
build_nonmem_sim_model <- function(
    model,
    seed,
    n_iterations,
    update_table = TRUE,
    variables = NULL,
    output_file = "simtab",
    verbose = TRUE
) {
  ## Set simulation (pharmr::set_simulation() modifies the model that sometimes
  ## invalidate the model, so add manually)
  if(verbose) cli::cli_alert_info("Changing model to simulation-only model")
  sim_model <- model |>
    set_simulation_clean(seed = seed, n = n_iterations)

  if(!update_table) {
    if(verbose) cli::cli_alert_info("Using existing table record(s)")
    return(sim_model)
  }

  if(verbose) cli::cli_alert_info("Updating table record(s)")
  parameter_names <- get_defined_pk_parameters(sim_model)
  if(is.null(variables)) {
    default_variables <- c("ID", "TIME", "DV", "EVID", "PRED")
    covariate_names <- vapply(
      pharmr::get_model_covariates(sim_model),
      function(x) x$name,
      character(1)
    )
    variables <- c(
      default_variables, get_declared_variables(sim_model), covariate_names
    )
  }
  checked_variables <- c()
  for(variab in variables) {
    check_var <- check_nm_table_variables(sim_model, variab, throw_error = FALSE)
    if(is.null(check_var)) { # i.e. IPRED is declared as variable and we can safely add to table
      checked_variables <- c(checked_variables, variab)
    }
  }
  ## Bioavailability too: AUC_SS is F * dose / CL, and F is individual
  ## wherever it carries IIV, so it has to come from the table just like CL.
  ## NONMEM names are case-insensitive, so `f1` counts as well.
  bioavailability_names <- get_defined_pk_parameters(
    sim_model, possible = c(paste0("F", 1:99), paste0("f", 1:99))
  )
  table_variables <- unique(
    c(checked_variables, parameter_names, bioavailability_names)
  )
  sim_model |>
    remove_tables_from_model(reload_dataset = FALSE) |>
    add_table_to_model(table_variables, file = output_file, reload_dataset = FALSE)
}

#' Prepare what every replicate's run folder is built from
#'
#' The draw-independent half of the prepare step: the per-regimen datasets
#' (written once and copied into every run folder) and the simulation model the
#' draws are applied to. Split out from [prepare_nonmem_replicate_spec()] so the
#' sequential path can prepare its replicates one at a time. Preparing all of
#' them up front costs a run folder plus a full copy of the simulation dataset
#' per replicate per regimen on disk before the first NONMEM starts, which the
#' parallel path needs -- the specs are what travels to the workers -- but a
#' path that consumes them strictly in order does not.
#'
#' The caller owns the temporary per-regimen datasets: `unlink()` the
#' `dataset_files` element once the last replicate has been prepared.
#'
#' @param model the Pharmpy model to simulate (point estimates; the draws are
#' applied per replicate, by [prepare_nonmem_replicate_spec()]).
#' @param draws data.frame of parameter draws, used here only to check that its
#' columns name parameters the rendered simulation model still has.
#' @param regimens the output of [resolve_sim_regimens()].
#' @inheritParams build_nonmem_sim_model
#'
#' @returns a list with `sim_model` (the simulation model the draws go into),
#' `regimens` (as passed, plus a `file` per regimen), `table_names` (the
#' `$TABLE` files to read back) and `dataset_files` (the temporary CSVs the
#' caller has to clean up).
#' @noRd
prepare_nonmem_replicate_context <- function(
    model,
    draws,
    regimens,
    seed,
    n_iterations,
    update_table = TRUE,
    variables = NULL,
    output_file = "simtab",
    verbose = TRUE
) {
  ## One CSV per regimen, written once and copied into every replicate's run
  ## folder by prepare_run_folder(): the dataset is identical across draws.
  ## A copy per run folder rather than one shared file referenced by an
  ## absolute path, because NM-TRAN truncates the `$DATA` filename field.
  regimens <- lapply(regimens, function(reg) {
    reg$file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(reg$data, reg$file, quote = FALSE, row.names = FALSE)
    reg
  })

  ## Simulation model first, draw second -- the reverse of the order the
  ## sequential engine used to apply them. The two commute (the draw rewrites
  ## $THETA/$OMEGA/$SIGMA values, the simulation setup rewrites $ESTIMATION,
  ## $SIMULATION and $TABLE), and this way the expensive half happens once:
  ## `set_simulation_clean()` round-trips the model through Pharmpy and rewrites
  ## its dataset reference, which for 16 draws of a small model was over a third
  ## of the whole run.
  sim_model <- build_nonmem_sim_model(
    model        = model,
    seed         = seed,
    n_iterations = n_iterations,
    update_table = update_table,
    variables    = variables,
    output_file  = output_file,
    verbose      = verbose
  )

  ## Which tables to read back. Taken from the rendered control stream rather
  ## than from `output_file`, so `update_table = FALSE` (tables as the model
  ## declares them) works too. Resolved here because the workers only get the
  ## folder, not the model.
  table_names <- get_tables_in_model_code(sim_model$code)
  if(length(table_names) == 0) {
    cli::cli_abort(c(
      "The simulation model has no $TABLE record.",
      i = "Nothing would be written for the uncertainty replicates to be \\
           read back from."
    ))
  }

  check_draws_against_model(sim_model, draws)

  list(
    sim_model     = sim_model,
    regimens      = regimens,
    table_names   = table_names,
    dataset_files = vapply(regimens, function(reg) reg$file, character(1))
  )
}

#' Check that the draws name parameters the simulation model still has
#'
#' The draws are applied to the *rendered* simulation model, while their names
#' come from the fit of the model that was rendered. Values are unaffected by
#' that ordering, but names are not guaranteed to be: a parameter named through
#' the Pharmpy API without a matching `$THETA`/`$OMEGA` comment in the control
#' stream comes back from the round trip under a different name, and
#' `pharmr::set_initial_estimates()` would then reject every draw. One explained
#' error before any run folder is written beats `n_uncertainty` unexplained
#' ones.
#'
#' @param sim_model the rendered simulation model.
#' @param draws data.frame of parameter draws.
#'
#' @returns `NULL`, invisibly. Called for its side effect of aborting.
#' @noRd
check_draws_against_model <- function(sim_model, draws) {
  known <- unlist(sim_model$parameters$names)
  unknown <- setdiff(names(draws), known)
  if(length(unknown) == 0) return(invisible(NULL))
  cli::cli_abort(c(
    "Sampled parameter{?s} {.val {unknown}} {?is/are} not in the simulation model.",
    i = "The simulation model is the fitted model re-read from its control \\
         stream; parameter names assigned through the Pharmpy API do not \\
         always survive that round trip.",
    i = "Model parameters: {.val {known}}"
  ))
}

#' Prepare one replicate's run folders
#'
#' The draw-dependent half of the prepare step, and the only part that needs
#' Pharmpy per replicate: the draw is applied to the context's simulation model
#' and the result is written out (control stream + dataset) into
#' `<path>/<id>/uncertainty_<r>/regimen_<i>` by [prepare_run_folder()] -- the
#' same function [run_nlme()] uses, so the run folder is laid out exactly as a
#' normal run's.
#'
#' @param ctx the output of [prepare_nonmem_replicate_context()].
#' @param draw one row of the draws data.frame.
#' @param index 1-based replicate index.
#' @param id base run id.
#' @param path folder the run id is created under.
#'
#' @returns one replicate spec: `index`, `table_names` (the `$TABLE` files to
#' read back) and `regimens` (per regimen: `label`, `folder`, `model_file`,
#' `output_file`, `regimen_for_pk`). Plain R data only -- the specs are what
#' travels to the worker processes.
#' @noRd
prepare_nonmem_replicate_spec <- function(ctx, draw, index, id, path) {
  draw_model <- pharmr::set_initial_estimates(
    ctx$sim_model, inits = as.list(draw)
  )

  reg_specs <- lapply(ctx$regimens, function(reg) {
    obj <- prepare_run_folder(
      id = file.path(id, paste0("uncertainty_", index),
                     paste0("regimen_", reg$index)),
      model = draw_model,
      path = path,
      data = reg$file,
      force = TRUE,
      auto_stack_encounters = FALSE,
      copy_dataset = TRUE,
      ## Quiet whatever the caller's `verbose`: this runs once per replicate
      ## per regimen, and the run folder it would report on is bookkeeping of
      ## the uncertainty run rather than something the caller asked for.
      verbose = FALSE
    )
    list(
      label          = reg$label,
      folder         = normalizePath(obj$fit_folder, mustWork = TRUE),
      model_file     = obj$model_file,
      output_file    = obj$output_file,
      regimen_for_pk = reg$regimen_for_pk
    )
  })

  list(index = index, table_names = ctx$table_names, regimens = reg_specs)
}

#' Prepare a run folder per replicate and regimen, all of them up front
#'
#' [prepare_nonmem_replicate_context()] followed by one
#' [prepare_nonmem_replicate_spec()] per draw: what the parallel path needs,
#' the specs having to exist before they can be handed to the workers.
#'
#' Preparing everything up front also fails fast: an unwritable path or a model
#' Pharmpy cannot render is one error before any NONMEM starts, rather than
#' `n_uncertainty` worker failures.
#'
#' @inheritParams prepare_nonmem_replicate_context
#' @param id base run id.
#' @param path folder the run id is created under.
#'
#' @returns a list with one spec per draw, as
#' [prepare_nonmem_replicate_spec()] returns them.
#' @noRd
prepare_nonmem_replicate_specs <- function(
    model,
    draws,
    regimens,
    id,
    path,
    seed,
    n_iterations,
    update_table = TRUE,
    variables = NULL,
    output_file = "simtab",
    verbose = TRUE
) {
  ctx <- prepare_nonmem_replicate_context(
    model        = model,
    draws        = draws,
    regimens     = regimens,
    seed         = seed,
    n_iterations = n_iterations,
    update_table = update_table,
    variables    = variables,
    output_file  = output_file,
    verbose      = verbose
  )
  on.exit(unlink(ctx$dataset_files), add = TRUE)

  lapply(seq_len(nrow(draws)), function(r) {
    prepare_nonmem_replicate_spec(
      ctx = ctx, draw = draws[r, , drop = FALSE], index = r,
      id = id, path = path
    )
  })
}

#' Build the worker function that runs one NONMEM uncertainty replicate
#'
#' A factory rather than an inline closure, for the same reason as
#' `make_nlmixr_replicate_fn()`: the closure is serialised to the worker
#' together with its enclosing environment, and [run_sim()]'s frame holds the
#' Pharmpy `model`/`fit` (Python objects that must not be sent to a worker).
#' This frame holds a path and two flags.
#'
#' @param nmfe path to the nmfe script, resolved by the caller while Python is
#' still reachable.
#' @param n_iterations number of `$SIMULATION` subproblems, to tell apart in
#' the output table (see `tag_sim_subproblems()`).
#' @param update_table were the `$TABLE` records rebuilt by [run_sim()]?
#' @param add_pk_variables add derived PK variables to the output table?
#' @param dv_scale multiplier putting AUC_SS into the units the model reports
#' concentrations in (see [get_dv_scale_factor()]).
#' @param clean remove NONMEM's temporary files from each run folder after the
#' run, as [run_nlme()] does? One folder per replicate per regimen is a lot of
#' scratch to leave behind.
#'
#' @returns a function taking one replicate spec and returning its
#' [run_captured()] envelope, whose result is that replicate's simulation
#' output across all regimens (with `regimen_label`).
#' @noRd
make_nonmem_replicate_fn <- function(
    nmfe,
    n_iterations = 1,
    update_table = TRUE,
    add_pk_variables = FALSE,
    dv_scale = 1,
    clean = TRUE
) {
  force(nmfe)
  force(n_iterations)
  force(update_table)
  force(add_pk_variables)
  force(dv_scale)
  force(clean)
  function(spec) {
    run_captured(spec$index, function() {
      suppressMessages(
        lapply(spec$regimens, function(reg) {
          tab <- run_nonmem_sim_folder(
            spec        = reg,
            nmfe        = nmfe,
            table_names = spec$table_names,
            clean       = clean
          )
          if(update_table) {
            tab <- tag_sim_subproblems(
              tab, file.path(reg$folder, spec$table_names[1]), n_iterations
            )
          }
          if(update_table && add_pk_variables) {
            tab <- calc_pk_variables(tab, regimen = reg$regimen_for_pk,
                                     dv_scale = dv_scale)
          }
          tab |>
            dplyr::mutate(regimen_label = reg$label)
        }) |>
          dplyr::bind_rows()
      )
    })
  }
}

#' Run one prepared simulation run folder and read its table back
#'
#' The execute half: pure R, so it runs happily in a worker process. Same steps
#' [run_nlme()] takes for a simulation model — run NONMEM, clean up the scratch
#' files, read the output tables — minus the results parsing a simulation has
#' nothing to parse.
#'
#' @param spec one regimen's entry of a replicate spec (`label`, `folder`,
#' `model_file`, `output_file`).
#' @param nmfe path to the nmfe script.
#' @param table_names `$TABLE` files to read back, in model order.
#' @param clean remove NONMEM's temporary files afterwards?
#'
#' @returns the first output table, as a data.frame.
#' @noRd
run_nonmem_sim_folder <- function(spec, nmfe, table_names, clean = TRUE) {
  ## A prepared run folder can be executed more than once: parallel_lapply()
  ## retries, and falls back to running sequentially after a cluster failure
  ## (#134), over folders a failed attempt may already have written tables
  ## into. Clear them first, so a rerun whose NONMEM fails silently aborts on
  ## the missing table below instead of returning the previous attempt's.
  unlink(file.path(spec$folder, table_names))

  call_nmfe(
    model_file  = spec$model_file,
    output_file = spec$output_file,
    path        = spec$folder,
    nmfe        = nmfe,
    console     = FALSE,
    verbose     = FALSE
  )
  if(clean) clean_nonmem_folder(spec$folder)

  tables <- get_tables_from_folder(table_names, spec$folder)
  tab <- if(length(tables) > 0) tables[[1]] else NULL
  if(is.null(tab) || nrow(tab) == 0) {
    ## Neither pharmpy nor nmfe raise when a simulation writes no output table,
    ## so surface the .lst error here instead of returning an empty replicate.
    abort_on_failed_sim(
      regimen_label = spec$label,
      fit_folder = spec$folder
    )
  }
  tab
}

#' Abort on the first failed replicate of a NONMEM run
#'
#' NONMEM replicate failures are typically systematic (licence, no output
#' table, a control stream NM-TRAN rejects), so a short set of draws is more
#' likely to be a broken run than an unlucky one. The sequential path stops at
#' the failure; the parallel path has no such option -- the other workers are
#' already running -- so it checks once everything is back.
#'
#' @param replicates list of [run_captured()] envelopes.
#'
#' @returns `NULL`, invisibly. Called for its side effect of aborting.
#' @noRd
abort_on_failed_replicates <- function(replicates) {
  failed <- Filter(function(x) inherits(x$result, "condition"), replicates)
  if(length(failed) == 0) return(invisible(NULL))
  for(repl in failed) emit_replicate_warnings(repl$index, repl$warnings)
  cli::cli_abort(
    "Uncertainty replicate {failed[[1]]$index} failed.",
    parent = failed[[1]]$result
  )
}
