#' Add SIR sampling in covariance step in Pharmpy model
#' 
#' @inheritParams run_nlme
#' @param options SIR options, one of `iter`, `samples`
#' 
#' @export
#' 
add_sir <- function(
  model, 
  options = list(
    niter = 1,
    samples = 1000
  )
) {
  has_cov <- has_covariance_record(model)
  if(! inherits(options, "list")) {
    cli::cli_abort("`options` should be a `list`.")
  } else {
    if(! has_cov) {
      cli::cli_abort("Model need a $COVARIANCE step to use SIR.")
    }
    if(isFALSE(options$niter > 0)) {
      cli::cli_alert_info("Not running SIR (`niter` <= 0).")
      return(model)
    }
    if(! all(c("samples", "niter") %in% c(names(options)))) {
      cli::cli_abort("`add_sir(options=...)` argument requires T/F or a list with `samples` and `iter` elements.")
    }
    tool <- get_tool_from_model(model)
    if(tool == "nonmem") {
      cov_record <- paste0(get_covariance_record(model), collapse = "")
      model <- update_covariance_record(
        model, 
        glue::glue(cov_record, " SIRSAMPLE={options$samples} SIRNITER={options$niter} ")
      )
    }
  }
  model
}
