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
#' @noRd
resolve_sim_regimens <- function(data, input_data, verbose = TRUE) {
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
    ## Ensure column names & order matches
    if(all(names(reg_data) %in% names(input_data))) {
      reg_data <- reg_data[, names(input_data)]
    }
    list(
      index          = i,
      label          = reg_label,
      data           = reg_data,
      regimen_for_pk = sim_regimen_doses(reg_data)
    )
  })
}

#' The dosing regimen `calc_pk_variables()` needs, derived from a dataset
#'
#' AUC_SS is dose over CL, so the doses have to come from the simulation
#' dataset rather than from the model.
#'
#' @param data one regimen's simulation dataset.
#'
#' @returns `list(dose = )`, or `NULL` when the dataset has no dose records.
#' @noRd
sim_regimen_doses <- function(data) {
  if(!all(c("EVID", "AMT") %in% names(data))) return(NULL)
  dose_rows <- data[data$EVID == 1, , drop = FALSE]
  if(nrow(dose_rows) == 0) return(NULL)
  list(dose = dose_rows$AMT)
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
  table_variables <- unique(c(checked_variables, parameter_names))
  sim_model |>
    remove_tables_from_model(reload_dataset = FALSE) |>
    add_table_to_model(table_variables, file = output_file, reload_dataset = FALSE)
}

#' Prepare one run folder per replicate and regimen
#'
#' The prepare half: everything that needs Pharmpy happens here, in the parent.
#' Each replicate gets its draw applied, is turned into a simulation model, and
#' is written out (control stream + dataset) into
#' `<path>/<id>/uncertainty_<r>/regimen_<i>` by [prepare_run_folder()] — the
#' same function [run_nlme()] uses, so the run folder is laid out exactly as a
#' normal run's.
#'
#' Preparing everything up front also fails fast: an unwritable path or a model
#' Pharmpy cannot render is one error before any NONMEM starts, rather than
#' `n_uncertainty` worker failures.
#'
#' @param model the Pharmpy model to simulate (point estimates; the draws are
#' applied here).
#' @param draws data.frame of parameter draws, one row per replicate.
#' @param regimens the output of [resolve_sim_regimens()].
#' @param id base run id.
#' @param path folder the run id is created under.
#' @inheritParams build_nonmem_sim_model
#'
#' @returns a list with one spec per replicate: `index`, `table_names` (the
#' `$TABLE` files to read back) and `regimens` (per regimen: `label`, `folder`,
#' `model_file`, `output_file`, `regimen_for_pk`). Plain R data only — the
#' specs are what travels to the worker processes.
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
  ## One CSV per regimen, written once and copied into every replicate's run
  ## folder by prepare_run_folder(): the dataset is identical across draws.
  ## A copy per run folder rather than one shared file referenced by an
  ## absolute path, because NM-TRAN truncates the `$DATA` filename field.
  regimens <- lapply(regimens, function(reg) {
    reg$file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(reg$data, reg$file, quote = FALSE, row.names = FALSE)
    reg
  })
  on.exit(unlink(vapply(regimens, function(reg) reg$file, character(1))),
          add = TRUE)

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
    verbose      = FALSE
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

  lapply(seq_len(nrow(draws)), function(r) {
    draw_model <- pharmr::set_initial_estimates(
      sim_model, inits = as.list(draws[r, , drop = FALSE])
    )

    reg_specs <- lapply(regimens, function(reg) {
      obj <- prepare_run_folder(
        id = file.path(id, paste0("uncertainty_", r),
                       paste0("regimen_", reg$index)),
        model = draw_model,
        path = path,
        data = reg$file,
        force = TRUE,
        auto_stack_encounters = FALSE,
        copy_dataset = TRUE,
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

    list(index = r, table_names = table_names, regimens = reg_specs)
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
#' @param update_table were the `$TABLE` records rebuilt by [run_sim()]?
#' @param add_pk_variables add derived PK variables to the output table?
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
    update_table = TRUE,
    add_pk_variables = FALSE,
    clean = TRUE
) {
  force(nmfe)
  force(update_table)
  force(add_pk_variables)
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
          if(update_table && add_pk_variables) {
            tab <- calc_pk_variables(tab, regimen = reg$regimen_for_pk)
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
