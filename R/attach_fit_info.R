#' Attach fit info and tables to a fit object, e.g. from model fit or
#' Pharmpy grid search final results
#'
#' @inheritParams run_nlme
#' @inheritParams get_fit_info
#' @param fit_folder Folder the run was executed in, holding the output
#'   tables and the estimation output file.
#' @param is_sim_model Is `fit` the result of a simulation rather than an
#'   estimation? Simulation results have no fit summary and no residuals to
#'   repair.
#'
#' @returns The fit object with the model, the output tables and (for
#'   estimation runs) a fit summary attached as the `model`, `tables` and
#'   `info` attributes, and with `residuals` rebuilt into a joinable frame
#'   (see the `Value` section of [run_nlme()]).
#'
#'   Note that for a Pharmpy fit this is a *new* object — `ModelfitResults` is
#'   a frozen dataclass, so replacing `residuals` means replacing the object.
#'   Attributes already set on the fit passed in are carried over, but any
#'   reference the caller still holds points at the un-repaired fit.
#'
#' @export
attach_fit_info <- function(
  fit,
  model,
  fit_folder,
  output_file = "run.lst",
  is_sim_model = FALSE,
  verbose = TRUE
) {
  ## Read tables from the run folder
  ## Close by id throughout: a bare `cli_process_done()` pops whatever status
  ## is on cli's stack, which -- when the matching start was skipped -- is the
  ## caller's progress bar, and cli then errors on its teardown (issue #137).
  proc <- NULL
  if(verbose) proc <- cli::cli_process_start("Importing generated tables")
  tables <- get_tables_from_fit(
    model,
    fit_folder
  )
  if(!is.null(proc)) cli::cli_process_done(id = proc)

  ## Rebuild `residuals` so it is aligned with the observation records and
  ## carries join keys (see repair_residuals()). Must happen before the
  ## attributes are set: for Pharmpy fits this returns a new (immutable)
  ## results object, which would not carry attributes set on the old one.
  if(!is_sim_model) {
    fit <- repair_residuals(fit, model, tables, verbose = verbose)
  }

  ## Attach model object (with dataset) to fit, for traceability or use in post-processing
  attr(fit, "model") <- model

  ## Attach tables to model fit
  attr(fit, "tables") <- tables

  if(!is_sim_model) {
    ## Generate a summary of fit info
    proc <- NULL
    if(verbose) proc <- cli::cli_process_start("Summarizing fit results")
    fit_info <- get_fit_info(
      fit,
      path = fit_folder,
      output_file = output_file
    )
    attr(fit, "info") <- fit_info
    ## Inside the `!is_sim_model` branch, with the start it pairs with: sim
    ## models never open this status bar, so closing it here unconditionally
    ## used to pop the caller's bar instead.
    if(!is.null(proc)) cli::cli_process_done(id = proc)
  }

  fit
}
