#' Does the last estimation method in a model have maxeval=0?
#'
#' @export
#'
is_maxeval_zero <- function(model) {
  last_step <- model$execution_steps$to_dataframe() |>
    tail(1)
  options <- list()
  if(!is.null(last_step$tool_options) && length(last_step$tool_options) > 0) {
    options <- last_step$tool_options[[1]]
  }
  ((is.na(last_step$maximum_evaluations) && last_step$method == "FOCE") || isTRUE(last_step$maximum_evaluations == 0)) && (is.null(options$MAXEVAL) || isTRUE(options$MAXEVAL == "0"))
}
