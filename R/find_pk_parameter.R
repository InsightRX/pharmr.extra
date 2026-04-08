#' Find / match PK parameter based on generic name.
#'
#' E.g. a user may request `pharmr::remove_iiv("V")`, to remove
#' IIV on the central volume. But if in the model the central volume is actually
#' parametrized as `V1` or `V2`, then it will error. When wrapped in
#' `find_pk_parameter` this adds more safety. It will first look if the
#' parameter is used in the model as such. If not found directly, it will
#' attempt other common names for the parameter, depending on the ADVAN number
#'  of the model.
#'
#' @param parameter name of the parameter to find
#' @inheritParams run_nlme
#'
#' @returns TODO
#' 
#' @export
find_pk_parameter <- function(parameter, model) {
  ## first try if parameter exist as-is
  model_params <- pharmr::get_pk_parameters(model)
  if(as.character(parameter) %in% model_params) {
    return(parameter)
  }
  ## then, try to find depending on advan
  advan <- get_advan(model)
  if(advan %in% c(1, 3, 11)) {
    map <- list(
      "V"   = c("VC", "V1"),
      "V1"  = c("VC"),
      "VC"  = c("V1"),
      "Q"   = c("QP1"),
      "QP1" = c("Q"),
      "V2"  = c("VP1"),
      "VP1" = c("V2"),
      "V3"  = c("VP2"),
      "VP2" = c("V3")
    )
  } else {
    map <- list(
      "V"   = c("VC", "V2"),
      "V1"  = c("VC"),
      "V2"  = c("VC"),
      "VC"  = c("V2"),
      "Q"   = c("QP1"),
      "QP1" = c("Q"),
      "V3"  = c("VP1"),
      "VP1" = c("V3"),
      "V4"  = c("VP2"),
      "VP2" = c("V4")
    )
  }
  if(is.null(map[[parameter]])) {
    cli::cli_warn("Could not find parameter {parameter} in model as {parameter}, nor under different name.")
    return(parameter)
  } else {
    ## Try each candidate; prefer one that exists in model
    for(candidate in map[[parameter]]) {
      if(candidate %in% model_params) {
        cli::cli_alert_info("Found parameter {parameter} in model as {candidate}.")
        return(candidate)
      }
    }
    ## If none found in model, return the first candidate as best guess
    candidate <- map[[parameter]][1]
    cli::cli_alert_info("Found parameter {parameter} in model as {candidate}.")
    return(candidate)
  }
}
