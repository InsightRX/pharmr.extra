#' Attach fit info and tables to a fit object, e.g. from model fit or
#' Pharmpy grid search final results
#'
#' @inheritParams run_nlme
#' @inheritParams get_fit_info
#' @param fit_folder TODO
#' @param is_sim_model TODO
#'
#' @returns TODO
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
  if(verbose) cli::cli_process_start("Importing generated tables")
  tables <- get_tables_from_fit(
    model,
    fit_folder
  )
  if(verbose) cli::cli_process_done()

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
    if(verbose) cli::cli_process_start("Summarizing fit results")
    fit_info <- get_fit_info(
      fit,
      path = fit_folder,
      output_file = output_file
    )
    attr(fit, "info") <- fit_info
  }

  if(verbose) cli::cli_process_done()

  fit
}
