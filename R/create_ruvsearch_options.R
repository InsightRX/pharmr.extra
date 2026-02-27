#' Create options list for pharmpy `ruvsearch`
#'
#' See Pharmpy ruvsearch documentation for more info:
#' https://pharmpy.github.io/latest/ruvsearch.html
#'
#' @param skip character vector of residual error model types to exclude from
#'   the search. Valid values: `"IIV_on_RUV"`, `"power"`, `"combined"`,
#'   `"time_varying"`, `"additive"`. Default `NULL` means all are considered.
#' @param groups number of groups to use for the time-varying model (default 4)
#' @param p_value p-value threshold for model selection (default 0.001)
#' @param max_iter maximum number of search iterations (default 3)
#' @param dv integer index of the dependent variable column to evaluate.
#'   `NULL` uses the default DV.
#' @param strictness model selection strictness criteria string. `NULL` uses
#'   the Pharmpy default (`"minimization_successful or (rounding_errors and
#'   sigdigs>=0.1)"`).
#' @param parameter_uncertainty_method method for parameter uncertainty
#'   estimation. One of `"SANDWICH"`, `"SMAT"`, `"RMAT"`, `"EFIM"`, or `NULL`.
#'
#' @returns named list of options to pass as the `options` argument of
#'   `call_pharmpy_tool()` when `tool = "ruvsearch"`.
#'
#' @export
create_ruvsearch_options <- function(
  skip = NULL,
  groups = 4L,
  p_value = 0.001,
  max_iter = 3L,
  dv = NULL,
  strictness = NULL,
  parameter_uncertainty_method = NULL
) {

  valid_skip <- c("IIV_on_RUV", "power", "combined", "time_varying", "additive")
  if (!is.null(skip)) {
    invalid <- skip[!skip %in% valid_skip]
    if (length(invalid) > 0) {
      cli::cli_abort(c(
        "Invalid {.arg skip} value(s): {.val {invalid}}.",
        "i" = "Valid options are: {.val {valid_skip}}"
      ))
    }
  }

  valid_pum <- c("SANDWICH", "SMAT", "RMAT", "EFIM")
  if (!is.null(parameter_uncertainty_method) &&
      !parameter_uncertainty_method %in% valid_pum) {
    cli::cli_abort(c(
      "Invalid {.arg parameter_uncertainty_method}: {.val {parameter_uncertainty_method}}.",
      "i" = "Valid options are: {.val {valid_pum}}"
    ))
  }

  opts <- list(
    groups  = as.integer(groups),
    p_value = p_value,
    max_iter = as.integer(max_iter)
  )

  if (!is.null(skip) && length(skip) > 0) {
    opts$skip <- skip
  }
  if (!is.null(dv)) {
    opts$dv <- as.integer(dv)
  }
  if (!is.null(strictness)) {
    opts$strictness <- strictness
  }
  if (!is.null(parameter_uncertainty_method)) {
    opts$parameter_uncertainty_method <- parameter_uncertainty_method
  }

  opts

}
