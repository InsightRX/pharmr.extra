#' Update the covariance record in a Pharmpy NONMEM model
#' 
#' @inheritParams run_nlme
#' 
#' @param cov_record new covariance record
#' 
#' @export 
#' 
update_covariance_record <- function(model, cov_record) {
  data <- model$dataset
  current_cov <- get_covariance_record(model)
  model_code <- stringr::str_split(model$code, "\\n")[[1]]
  cov_idx <- which(stringr::str_detect(model_code, "^\\$COV"))
  if(length(cov_idx) > 0) {
    for(i in cov_idx) {
      model_code[i] <- paste0(cov_record, collapse = "\n")
    }
  } else {
    cli::cli_abort("Model need a $COVARIANCE step to use SIR.")
  }
  model <- pharmr::read_model_from_string(
    code = paste(model_code, collapse = "\n")
  )
  if(!is.null(data)) {
    model <- pharmr::set_dataset(model, data)
  }
  model
}

#' Extract covariance record from a Pharmpy NONMEM model object
#' 
#' @inheritParams run_nlme
#' 
#' @returns $COVARIANCE record (character)
#' 
#' @export
get_covariance_record <- function(model) {
  if(!inherits(model, "pharmpy.model.external.nonmem.model.Model")) {
    cli::cli_abort("`model` is not a Pharmpy NONMEM model object.")
  }
  obj <- nm_read_model(code = model$code)
  suppressWarnings(
    cov_record <- obj$COV  
  )
  paste0(cov_record, collapse = " ")
}

#' Does the Pharmpy NONMEM model object have a $COV record?
#' 
#' @inheritParams run_nlme
#' 
#' @returns TRUE or FALSE
#' 
#' @export
#' 
has_covariance_record <- function(model) {
  cov_record <- get_covariance_record(model)
  stringr::str_detect(cov_record, "\\$COV")[[1]]
}
