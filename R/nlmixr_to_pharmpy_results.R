#' Convert an nlmixr-shaped fit to native Pharmpy ModelfitResults
#'
#' @noRd
as_native_pharmpy_modelfit_results <- function(results) {
  if(inherits(results, "pharmpy.workflows.results.ModelfitResults")) {
    return(results)
  }
  if(!inherits(results, "nlmixr2_modelfit_results")) {
    return(NULL)
  }

  pd <- reticulate::import("pandas", convert = FALSE)
  workflows <- reticulate::import("pharmpy.workflows.results", convert = FALSE)
  log_mod <- reticulate::import("pharmpy.workflows.log", convert = FALSE)

  args <- list(
    ofv = scalar_or_none(results$ofv),
    ofv_iterations = series_or_none(results$ofv_iterations, pd = pd),
    parameter_estimates = series_or_none(results$parameter_estimates, pd = pd),
    covariance_matrix = dataframe_or_none(results$covariance_matrix, pd = pd),
    correlation_matrix = dataframe_or_none(results$correlation_matrix, pd = pd),
    precision_matrix = dataframe_or_none(results$precision_matrix, pd = pd),
    standard_errors = series_or_none(results$standard_errors, pd = pd),
    relative_standard_errors = series_or_none(results$relative_standard_errors, pd = pd),
    minimization_successful = scalar_or_none(results$minimization_successful),
    estimation_runtime = scalar_or_none(results$estimation_runtime),
    residuals = dataframe_or_none(results$residuals, pd = pd),
    predictions = dataframe_or_none(results$predictions, pd = pd),
    runtime_total = scalar_or_none(results$runtime_total),
    termination_cause = scalar_or_none(results$termination_cause),
    function_evaluations = scalar_or_none(results$function_evaluations),
    significant_digits = scalar_or_none(results$significant_digits),
    covstep_successful = scalar_or_none(results$covstep_successful),
    warnings = warnings_or_none(results$warnings),
    ## An empty `Log` rather than the `None` default: Pharmpy tools that
    ## summarize errors across model entries call `len(res.log)` without a
    ## None-check (`pharmpy.tools.run.summarize_errors_from_entries()`), so a
    ## missing log makes `modelsearch` and friends fail in post-processing
    ## (#121).
    log = log_mod$Log()
  )

  out <- do.call(workflows$ModelfitResults, args)
  attr(out, "model") <- attr(results, "model")
  attr(out, "final_model") <- attr(results, "final_model")
  attr(out, "tables") <- attr(results, "tables")
  out
}

#' @noRd
scalar_or_none <- function(x) {
  if(is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(reticulate::py_none())
  }
  if(is.logical(x)) return(isTRUE(x))
  if(is.numeric(x)) return(unname(as.numeric(x[[1]])))
  if(is.character(x)) return(unname(as.character(x[[1]])))
  x
}

#' @noRd
series_or_none <- function(x, pd) {
  if(is.null(x) || length(x) == 0) {
    return(reticulate::py_none())
  }
  if(is.data.frame(x)) {
    return(dataframe_or_none(x, pd = pd))
  }
  values <- unname(as.vector(x))
  index <- names(x)
  if(is.null(index)) index <- seq_along(values) - 1L
  pd$Series(data = values, index = index)
}

#' @noRd
dataframe_or_none <- function(x, pd) {
  if(is.null(x)) {
    return(reticulate::py_none())
  }
  if(is.matrix(x)) {
    df <- as.data.frame(x, check.names = FALSE)
    index <- rownames(x)
    if(is.null(index)) {
      return(pd$DataFrame(df))
    }
    return(pd$DataFrame(df, index = index))
  }
  if(is.data.frame(x)) {
    if(nrow(x) == 0 && ncol(x) == 0) return(reticulate::py_none())
    return(pd$DataFrame(x))
  }
  reticulate::py_none()
}

#' @noRd
warnings_or_none <- function(x) {
  if(is.null(x)) {
    return(reticulate::py_none())
  }
  as.character(x)
}
