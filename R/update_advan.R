#' Update the ADVAN number
#' 
#' Note: this only updates the ADVAN number in $SUBROUTINEs, but does not 
#' change anything in the remaining model code! The primary use case for this
#' function is for easy switching between ADVAN 6, 9, and 13 for ODE models.
#' 
#' @param model a Pharmpy NONMEM model object
#' @param advan new advan
#' 
#' @export 
#' 
update_advan <- function(model, advan) {
  data <- model$dataset
  model_code <- model$code
  if(! advan %in% c(6, 9, 13)) {
    cli::cli_abort("Only supported for ADVAN 6, 9, or 13.")
  }
  model_code <- model_code |>
    stringr::str_replace(
      "ADVAN\\d+",
      glue::glue("ADVAN{advan}")
    ) |>
    stringr::str_replace(
      "TRANS[0-9]",
      "TOL=9"
    )
  model <- pharmr::read_model_from_string(
    code = paste(model_code, collapse = "\n")
  )
  if(!is.null(data)) {
    model <- pharmr::set_dataset(model, data)
  }
  model
}
