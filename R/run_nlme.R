#' Run model in NONMEM
#'
#' Run the model directly using nmfe (not through pharmpy).
#' This is a more reliable way of running NONMEM, and it is now possible to
#' stream stdout and stderr to file or to console, which is useful for
#' inspection of intermediate model fit.
#'
#' The function does take a pharmpy model as input (optionally), and uses
#' pharmpy to read the results from the model fit, and returns a pharmpy
#' `modelfit` object.
#'
#' @param model pharmpy model object or NONMEM model code (character) or path
#' to NONMEM model file.
#' @param data filename of dataset or data.frame as input to NONMEM / nlmixr.
#' Optional, can also be included in `model` object (if specified as pharmpy
#' model object).
#' @param tables acharacter vector of which default tables
#' to add, options are `fit` and `parameters`. Default is NULL,
#' i.e. don't add any new tables (but will keep existing).
#' @param full_tables For the default tables, should all input columns from be
#' included in the output tables? Default `FALSE`.
#' @param id run id, e.g. `run1`. This will be the folder in which the NONMEM
#' model is run. If no folder is specified, it will create a folder `run1` in
#' the current working directory, and will increment the run number for each
#' subsequent run.
#' @param path path to nonmem model. If not specified, will assume current
#' working directory.
#' @param method run method, either `pharmpy` dispatch, `nmfe` or `psn`
#' (psn::execute).
#' @param nmfe full path to nmfe file to run NONMEM with, if `method=="nmfe"`.
#' @param console show stderr and stdout in R console? If FALSE, will stream
#' to files `stdout` and `stderr` in fit folder.
#' @param force if run folder (`id`) exists, should existing results be
#' removed before rerunning NONMEM? Default `FALSE`.
#' @param save_fit save fit object. If `TRUE`, will save as `<id>.rds` inside
#' `path`. Can also specify a filename (rds) to save to; relative filenames
#' are resolved against `path`, absolute ones are used as-is. `FALSE` writes
#' nothing.
#' @param save_summary save fit summary and parameter estimates to file?
#' Default is `TRUE`. Files are written to `path` as `<id>_fit_summary.txt`
#' and `<id>_fit_parameters.csv`.
#' @param estimation_method Optional. Character vector of estimation method(s)
#' to apply to model. Will remove all existing estimation steps in the model
#' and update with methods specified in argument.
#' @param estimation_options Optional. Options for the estimation step(s).
#' Either a flat named list (applied to the first step) or a named list of
#' lists keyed by method name for multi-step estimation, e.g.
#' `list(SAEM = list(NBURN = 500), IMP = list(NITER = 10))`. Options are
#' merged with package defaults; user values take precedence. Keys that
#' correspond to pharmpy structured fields (MAXEVAL, NITER, ISAMPLE, PRINT,
#' AUTO, ETASAMPLES) are routed to the appropriate attribute to avoid
#' duplication in the rendered `$EST` record.
#' @param sir_options options for running SIR in covariance step. A list with
#' options `niter` (number of SIR iterations) and `samples` (number of
#' samples). Default `NULL` leaves the model unchanged. `samples` should be
#' between 300 and 10000 (suggested to use 1000 by default). `niter` should be
#' 1 or higher (suggest to use 1 by default).
#' @param auto_stack_encounters only invoked if `data` argument supplied as
#' a data.frame, not if a pharmpy model object is supplied without `data` or
#' when `data` is a filename.
#' Detects if TIME within an individual is
#' decreasing from one record to another, which NONMEM cannot handle.
#' If this happens, it will add a reset event (EVID=3) at that time, and
#' increase the TIME for subsequent events so that NONMEM does not throw an
#' error. It will increase the time for the next encounter to the maximum
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same
#' individual ispresent but is included in the dataset as time-after-dose and
#' not actual time since first overall dose.
#' @param copy_dataset copy the dataset into the run folder? If `TRUE`, the
#' dataset is copied into the run folder as `data.csv` and the model's `$DATA`
#' record is rewritten to point to that copy. If `FALSE` (default), the dataset
#' is left in its existing location and the model's `$DATA` record is left
#' untouched (the caller is responsible for `$DATA` already pointing at the
#' dataset correctly). `copy_dataset = FALSE` can only be honored when the
#' dataset is a file on disk — i.e. `data` is supplied as a file path, or the
#' model's `$DATA` record points to an existing file. If neither is the case
#' (only an in-memory data frame, `model$dataset`, or original dataset is
#' available), a warning is issued and the dataset is copied into the run
#' folder (with `$DATA` rewritten) anyway.
#' @param clean clean up run folder after NONMEM execution?
#' @param as_job run as RStudio job?
#' @param save_final after running the model, should a file `final.mod` be created
#' with the final estimates from the run.
#' @param check_only if `TRUE`, will only check the model code (NM-TRAN in the case
#' of NONMEM), but not run the model. Will return `TRUE` if model syntax is
#' correct, and `FALSE` if not. Will also attach stdout as `message` attribute.
#' @param remove_tables if `TRUE`, removes all `$TABLE` records from the model
#' before running. Applied after any tables added via the `tables` argument.
#' Default is `FALSE`.
#' @param mu_reference Controls mu-referencing for SAEM models. `"auto"`
#' (default) automatically applies `pharmr::mu_reference_model()` when SAEM is
#' used and the model is not already mu-referenced. `TRUE` always applies
#' mu-referencing. `FALSE` never applies mu-referencing (old behaviour: warns
#' when SAEM is used without mu-referencing).
#' @param threads number of threads to use for MPI parallelization of a
#' single NONMEM run. `NULL` (default) or `1` runs single-threaded (no
#' parafile written). Values `>= 2` cause an MPI parafile (`parafile.pnm`)
#' to be written into the run folder and passed to the selected backend.
#' Requires MPI (e.g. OpenMPI) installed on the host running NONMEM, with
#' `mpirun` on the PATH. When `method = "pharmpy"` is combined with
#' `threads >= 2`, the run is dispatched via `nmfe` instead, since
#' pharmpy's API does not expose a parafile hook.
#' @param control nlmixr2-only. Optional control list passed verbatim to
#' [nlmixr2::nlmixr2()] (e.g. [nlmixr2est::foceiControl()] or
#' [nlmixr2est::saemControl()]). Ignored for NONMEM models.
#' @param verbose verbose output?
#'
#' @returns A Pharmpy `ModelfitResults` object (an nlmixr2-shaped fit list for
#'   nlmixr2 models), with the model, the output tables and a fit summary
#'   attached as the `model`, `tables` and `info` attributes. `predictions`
#'   holds one row per record of the model dataset; `residuals` holds one row
#'   per observation record, plus join keys: `ROW` (row number in the model
#'   dataset) and the model's ID and independent-variable columns. The latter
#'   two are taken from the model's datainfo, so they are named `ID` and `TIME`
#'   for a typical NONMEM dataset but follow the dataset when it uses other
#'   names (e.g. `SUBJ` / `TAD`). `ROW` is named `.ROW` in the rare case that
#'   the dataset already has a column called `ROW`.
#'
#' @export
run_nlme <- function(
  model,
  data = NULL,
  tables = NULL,
  full_tables = FALSE,
  id,
  path = getwd(),
  method = c("nmfe", "pharmpy", "psn"),
  nmfe = get_nmfe_location(),
  force = NULL,
  console = FALSE,
  save_fit = TRUE,
  save_summary = TRUE,
  estimation_method = NULL,
  estimation_options = NULL,
  sir_options = NULL,
  auto_stack_encounters = FALSE,
  copy_dataset = FALSE,
  clean = TRUE,
  as_job = FALSE,
  save_final = TRUE,
  check_only = FALSE,
  remove_tables = FALSE,
  mu_reference = "auto",
  threads = NULL,
  control = NULL,
  verbose = TRUE
) {

  time_start <- Sys.time()
  
  ## An in-memory data.frame has no on-disk "existing location" to reference,
  ## so it must always be written into the run folder regardless of
  ## `copy_dataset` (otherwise $DATA would point at the ephemeral tempfile
  ## created just below).
  data_in_memory <- inherits(data, "data.frame")

  ## Make sure `data` is pointing to a file. This is to avoid issue with
  ## Pharmpy trying to parse the data.frame. `data` may also be NULL, in
  ## which case `prepare_run_folder()` resolves the dataset from the model's
  ## $DATA record or `model$dataset`.
  if(!is.null(data)) {
    if(inherits(data, "data.frame")) {
      datafile <- tempfile(pattern = "data_", fileext = ".csv")
      write.csv(data, datafile, quote = FALSE, row.names = FALSE)
      data <- datafile
    } else if(!inherits(data, "character")) {
      cli::cli_abort("`data` is of unknown type.")
    }
  }

  ## Preserve R attributes across pharmpy calls (which create new Python objects)
  original_data <- attr(model, "original_data")
  model <- validate_model(model, data = data)
  method <- match.arg(method)

  ## Engine dispatch: nlmixr-format models go through a separate fitter
  ## that calls nlmixr2 directly. Pharmpy-driven nlmixr fitting needs the
  ## Python `pyreadr` package which is not part of the standard install,
  ## and the direct path avoids an R→Python→R round-trip.
  if(get_tool_from_model(model) == "nlmixr") {
    return(run_nlme_nlmixr(
      model = model,
      data = data,
      id = id,
      path = path,
      estimation_method = estimation_method,
      control = control,
      force = force,
      save_fit = save_fit,
      save_summary = save_summary,
      save_final = save_final,
      clean = clean,
      mu_reference = mu_reference,
      verbose = verbose
    ))
  }

  ## Set model name
  model <- pharmr::set_name(
    model = model,
    new_name = id
  )

  ## Change estimation method, if requested
  if(!is.null(estimation_method)) {
    per_step_options <- if(!is.null(estimation_options)) {
      parse_estimation_options(estimation_method, estimation_options)
    } else {
      NULL
    }
    model <- update_estimation_method(
      model,
      estimation_method,
      per_step_options = per_step_options,
      verbose = verbose
    )
  }

  ## Add SIR to covariance step, if requested
  if(!is.null(sir_options)) {
    model <- add_sir(model, options = sir_options)
  }

  ## Apply mu-referencing based on `mu_reference` argument
  steps <- model$execution_steps$to_dataframe()
  is_saem <- "saem" %in% tolower(steps$method)
  is_mu_ref <- pharmr::has_mu_reference(model)
  if((isTRUE(mu_reference) && !is_mu_ref) || (identical(mu_reference, "auto") && is_saem && !is_mu_ref)) {
    cli::cli_alert_info("Applying mu-referencing to model.")
    model <- pharmr::mu_reference_model(model)
  } else if(isFALSE(mu_reference) && is_saem && !is_mu_ref) {
    cli::cli_warn(
      "Model uses SAEM but is not mu-referenced. Consider setting {.code mu_reference = \"auto\"} for better convergence."
    )
  }

  ## Add default tables, if requested
  if(!is.null(tables)) {
    model <- add_default_output_tables(
      model = model,
      tables = tables,
      full_tables = full_tables
    )
  }

  ## Remove $TABLE records, if requested
  if(remove_tables) {
    if(verbose) cli::cli_alert_info("Removing $TABLE records from model")
    model <- remove_tables_from_model(model)
  }

  ## Restore original_data attribute (lost by pharmpy calls above)
  if(!is.null(original_data)) {
    attr(model, "original_data") <- original_data
  }

  ## Make sure data is clean for modelfit
  obj <- prepare_run_folder(
    id = id,
    model = model,
    path = path,
    data = data,
    force = force,
    auto_stack_encounters = auto_stack_encounters,
    copy_dataset = copy_dataset || data_in_memory,
    verbose = verbose
  )

  ## If only `check` requested:
  if(check_only) {
    model_ok <- call_nmfe(
      model_file = obj$model_file,
      output_file = obj$output_file,
      path = obj$fit_folder,
      nmfe = nmfe,
      check_only = TRUE,
      console = console,
      verbose = verbose
    )
    return(model_ok)
  }

  ## Generate MPI parafile if multi-threaded run requested
  parafile <- NULL
  if(!is.null(threads) && threads >= 2) {
    parafile <- create_mpi_parafile(path = obj$fit_folder, threads = threads)
    if(verbose) {
      cli::cli_alert_info(
        "Wrote MPI parafile {.path {parafile}} with [nodes]={threads}"
      )
    }
    if(method == "pharmpy") {
      cli::cli_warn(
        "Pharmpy backend does not support parafiles; falling back to method = 'nmfe'."
      )
      method <- "nmfe"
    }
  }

  ## Run NONMEM and direct stdout/stderr
  if(method == "pharmpy") {
    if(as_job) {
      if(! rstudioapi::isAvailable()) {
        cli::cli_abort("RStudio API not available, cannot start job.")
      }
      suppressMessages({
        jobid <- job::job(
          title = paste0(id, "-", "modelfit"),
          {
            call_pharmpy_fit(
              model_file = obj$model_file,
              path = obj$fit_folder,
              verbose = verbose,
              console = console
            )
          }
        )
      })
      cli::cli_alert_info("Job with id {jobid} started")
      return(invisible(jobid))
    } else {
      call_pharmpy_fit(
        model_file = obj$model_file,
        path = obj$fit_folder,
        verbose = verbose,
        console = console
      )
    }
  } else if(method ==  "nmfe") {
    if(as_job) {
      cli::cli_alert_warning("Sorry, running as job not implemented yet for nmfe runs.")
    }
    call_nmfe(
      model_file = obj$model_file,
      output_file = obj$output_file,
      path = obj$fit_folder,
      nmfe = nmfe,
      console = console,
      verbose = verbose,
      parafile = parafile,
      threads = threads
    )
  } else if(method == "psn") {
    if(as_job) {
      cli::cli_alert_warning("Sorry, running as job not implemented yet for PsN runs.")
    }
    call_psn(
      model_file = obj$model_file,
      output_file = obj$output_file,
      path = obj$fit_folder,
      tool = "execute",
      console = console,
      verbose = verbose,
      parafile = parafile,
      threads = threads
    )
  } else{
    cli::cli_abort("Model run method {method} not recognized.")
  }

  if(clean) {
    if(verbose) cli::cli_alert_info("Cleaning up run folder")
    clean_nonmem_folder(obj$fit_folder)
  }

  ## Check if sim / eval model only
  is_sim_model <- pharmr::is_simulation_model(model)
  is_eval_model <- is_maxeval_zero(model)
  if(is_sim_model || is_eval_model) {
    fit <- list(
      ## just return empty list for now
    )
  } else {
    ## Read results using Pharmpy and return
    parse_proc <- NULL
    if(verbose) parse_proc <- cli::cli_process_start("Parsing results from run")
    fit <- pharmr.extra::read_modelfit_results( ## pharmr.extra drop-in replacement. Original has bug with reading SIR results
      file.path(obj$fit_folder, obj$model_file)
    )
    if(!is.null(parse_proc)) cli::cli_process_done(id = parse_proc)
    if(is.null(fit)) {
      if(verbose) {
        if(!console) {
          cli::cli_alert_danger("Something went wrong with fit. Output shown below.")
          nmfe_output <- get_nmfe_output(
            path = obj$fit_folder,
            obj$output_file
          )
          print_nmfe_output(nmfe_output)
        }
      }
      cli::cli_abort("No results from modelfit, please check run output.")
    }
  }

  ## Re-read the model from disk so it carries the correct file path.
  ## This is important for downstream tools (e.g. ruvsearch) that resolve
  ## output table paths (sdtab, etc.) via the model's stored filename.
  model_on_disk <- pharmr::read_model(file.path(obj$fit_folder, obj$model_file))

  ## Attach fit info / tables as attributes, also for simulation
  fit <- attach_fit_info(
    fit,
    model = model_on_disk,
    obj$fit_folder,
    obj$output_file,
    is_sim_model = is_sim_model,
    verbose = verbose
  )

  if(!is_sim_model) {
    ## Create final.mod with updated estimates?
    if(save_final) {
      final_model <- update_parameters(obj$model, fit)
      if(!is.null(final_model)) {
        if(verbose) {
          cli::cli_alert_info("Saving model with updated estimates to final.mod")
        }
        attr(fit, "final_model") <- final_model
        final_model_code <- final_model$code
        final_model_code <- change_nonmem_dataset(final_model_code, obj$dataset_path)
        writeLines(final_model_code, file.path(obj$fit_folder, "final.mod"))
      } else {
        if(verbose) {
          cli::cli_alert_warning("Final parameter estimates not available, not saving final.mod")
        }
      }
    }

    ## save fit object to file. Relative names resolve against `path`, not
    ## the working directory, so a caller that runs in a temp/analysis folder
    ## doesn't get stray files next to their R session.
    if(!is.null(save_fit)){
      if(inherits(save_fit, "character")) {
        saveRDS(fit, resolve_output_file(save_fit, path))
      } else if(inherits(save_fit, "logical")) {
        if(save_fit) {
          saveRDS(fit, resolve_output_file(paste0(id, ".rds"), path))
        }
      }
    }

    ## save fit summary (fit info and parameter estimates) as JSON
    if(save_summary) {
      save_proc <- NULL
      if(verbose) save_proc <- cli::cli_process_start("Saving fit results to file")
      fit_summ <- create_modelfit_info_table(fit)
      txt_summ <- knitr::kable(fit_summ, row.names = FALSE, format = "simple")
      writeLines(
        txt_summ,
        resolve_output_file(paste0(id, "_fit_summary.txt"), path)
      )
      par_est <- create_modelfit_parameter_table(fit)
      write.csv(
        par_est,
        resolve_output_file(paste0(id, "_fit_parameters.csv"), path),
        quote=F, row.names=F
      )
      if(!is.null(save_proc)) cli::cli_process_done(id = save_proc)
    }
  }

  time_end <- Sys.time()
  time_all <- round(as.numeric(time_end - time_start), 1)
  if(verbose) cli::cli_alert_success(paste0("Run done (", time_all,"s)."))

  ## Expose the resolved run folder so callers (e.g. run_sim) can locate
  ## on-disk output without re-deriving it from `path`/`id` defaults.
  attr(fit, "fit_folder") <- obj$fit_folder

  fit

}

#' Get new run number for model fit
#'
#' @param path path to folder in which to create subfolder for run
#'
get_new_run_number <- function(path = getwd()) {
  folders <- stringr::str_replace_all(
    dir(path, include.dirs = TRUE, pattern = "^run[0-9].?$"),
    "run",
    ""
  )
  numbers <- as.numeric(folders)
  if(length(numbers) == 0) {
    new_number <- 1
  } else {
    new_number <- max(numbers) + 1
  }
  new_number
}

#' Change $DATA in NONMEM model code
#'
#' Thin wrapper around [update_nonmem_data()] kept for internal callers.
#' Always returns a single string regardless of the input shape.
#'
#' @param code model code, either as single line string, or vector of lines
#' @param path path of new dataset
#'
change_nonmem_dataset <- function(code, path) {
  out <- update_nonmem_data(code, path)
  if (length(out) > 1) out <- paste(out, collapse = "\n")
  out
}

#' Call nmfe
#'
#' @param model_file model file, e.g. "run.mod"
#' @param output_file output file, e.g. "run.lst"
#' @param path run folder path, e.g. "run1"
#' @param nmfe path to nmfe batch file to run NONMEM
#' @param console show output from nmfe in console? Default `FALSE`
#' @param check_only only run NM-TRAN, to check the model syntax
#' @param verbose verbose output?
#' @param parafile absolute path to a NONMEM parafile (MPI or FPI). If
#' supplied, will be passed to nmfe as `-parafile=<path>`. Default `NULL`
#' (no parafile).
#' @param threads number of nodes to request, passed to nmfe as
#' `[nodes]=N`. Only applied when `parafile` is supplied.
#'
#' @export
#'
call_nmfe <- function(
  model_file,
  output_file,
  path,
  nmfe = "/opt/NONMEM/nm_cxurrent/run/nmfe75",
  console = FALSE,
  check_only = FALSE,
  verbose = FALSE,
  parafile = NULL,
  threads = NULL
) {

  if(! file.exists(nmfe)) {
    cli::cli_abort("NONMEM (nmfe) not found at {nmfe}")
  } else {
    if(verbose) {
      cli::cli_alert_success("NONMEM found at {nmfe}")
    }
  }

  # Transform folder path to absolute path
  path <- normalizePath(path, mustWork = TRUE)

  ## Keep the status-bar id: `cli_process_done()` without one closes whatever
  ## status happens to be on cli's stack, which -- when `verbose = FALSE` and
  ## no status was opened here at all -- is the caller's progress bar. cli then
  ## errors on that bar's next update or on its teardown. See issue #137.
  nmfe_proc <- NULL
  if(verbose) {
    nmfe_proc <- cli::cli_process_start(
      paste0("Starting NONMEM (nmfe) run in ", path),
      on_exit = "failed"
    )
  }

  ## Output to console or to file?
  if(console) {
    stdout <- ""
    stderr <- ""
  } else {
    stdout <- file.path(path, "stdout")
    stderr <- file.path(path, "stderr")
  }
  curr_dir <- getwd()
  ## `add = TRUE`: a bare `on.exit()` would replace the deferred handler that
  ## `cli_process_start()` registered on this frame, leaking the status bar.
  on.exit(setwd(curr_dir), add = TRUE)
  setwd(path)
  if(check_only) {
    nmtran <- get_nmtran_from_nmfe(nmfe)
    if(!file.exists(nmtran)) {
      cli::cli_abort("NM-TRAN executable could not be found, can't perform syntax check.")
    }
    system2(
      command = nmtran,
      args = c("<", model_file),
      wait = TRUE,
      stdout = stdout,
      stderr = stderr
    )
    cons <- c(
      readLines(stderr),
      readLines(stdout)
    )
    has_no_error <- !any(stringr::str_detect(cons, "AN ERROR WAS FOUND"))
    attr(has_no_error, "message") <- cons
    if(!is.null(nmfe_proc)) {
      ## Close the bar to match the outcome we are about to return: a control
      ## stream NM-TRAN rejects must not print a success message.
      if(has_no_error) {
        cli::cli_process_done(id = nmfe_proc)
      } else {
        cli::cli_process_failed(id = nmfe_proc)
      }
    }
    return(has_no_error)
  } else {
    nmfe_args <- c(model_file, output_file)
    if(!is.null(parafile)) {
      nmfe_args <- c(nmfe_args, paste0("-parafile=", parafile))
      if(!is.null(threads)) {
        nmfe_args <- c(nmfe_args, paste0("[nodes]=", as.integer(threads)))
      }
    }
    system2(
      command = nmfe,
      args = nmfe_args,
      wait = TRUE,
      stdout = stdout,
      stderr = stderr,
    )
  }
  if(!is.null(nmfe_proc)) cli::cli_process_done(id = nmfe_proc)
}

#' Get the location of NM-TRAN based on the location of nmfe
#' It's usually up one folder from nmfe, then in tr/NMTRAN.exe
#'
#' @param nmfe TODO
get_nmtran_from_nmfe <- function(nmfe) {
  nm_folder <- dirname(dirname(nmfe))
  nmtran <- file.path(nm_folder, "tr", "NMTRAN.exe")
  nmtran
}

#' Get output from NMFE
#'
#' @param path path to folder with NMFE run
#' @param results_file name of output file
#'
get_nmfe_output <- function(path, results_file = "run.lst") {
  out <- list(
    stderr = NULL,
    stdout = NULL
  )
  if(file.exists(file.path(path, "stderr"))) {
    out$stderr <- readLines(file.path(path, "stderr"))
  }
  if(file.exists(file.path(path, "stdout"))) {
    out$stdout <- readLines(file.path(path, "stdout"))
  }
  if(file.exists(file.path(path, results_file))) {
    out$results_file <- readLines(file.path(path, results_file))
  }
  out
}

#' Print nmfe output (stdout and stderr) from a run folder
#'
#' @param nmfe_output output from nmfe command, as list
#'
print_nmfe_output <- function(
  nmfe_output
) {
  if(length(nmfe_output$stderr) > 0) {
    cli::cli_alert_warning("stderr: ")
    cat(paste0(nmfe_output$stderr, collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("stderr: <empty>")
  }
  if(length(nmfe_output$stdout) > 0) {
    cli::cli_alert_warning("stdout (last 10 lines): ")
    cat(paste0(utils::tail(nmfe_output$stdout, 10), collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("stdout: <empty>")
  }
  if(length(nmfe_output$results_file) > 0) {
    cli::cli_alert_warning("results file (last 10 lines): ")
    cat(paste0(utils::tail(nmfe_output$results_file, 10), collapse = "\n"), "\n\n")
  } else {
    cli::cli_alert_warning("results_file: <empty>")
  }
}
