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
  if(! inherits(options, "list")) {
    cli::cli_abort("`options` should be a `list`.")
  }
  if(isFALSE(options$niter > 0)) {
    return(model)
  }
  if(! all(c("samples", "niter") %in% c(names(options)))) {
    cli::cli_abort("`add_sir(options=...)` argument requires a list with `samples` and `niter` elements.")
  }

  tool <- get_tool_from_model(model)
  if(tool != "nonmem") {
    cli::cli_warn(c(
      "SIR (sampling-importance-resampling) is only supported for NONMEM models via PsN; ignoring `sir_options` for {tool} model.",
      i = "Consider {.fn nlmixr2est::bootstrapFit} or a Bayesian estimation step instead."
    ))
    return(model)
  }

  if(! has_covariance_record(model)) {
    cli::cli_abort("Model needs a $COVARIANCE step to use SIR.")
  }
  cov_record <- paste0(get_covariance_record(model), collapse = "")
  update_covariance_record(
    model,
    glue::glue(cov_record, " SIRSAMPLE={options$samples} SIRNITER={options$niter} ")
  )
}
