#' Create a model object from the model code and dataset stored as a list 
#' object.
#' 
#' @param model_obj list object with entries `code` and `dataset`
#' 
#' @returns (Model) Pharmpy model object
create_pharmpy_model_from_list <- function(model_obj) {
  ## Pharmpy bug: datainfo not updated when using pharmar::set_dataset()
  ## So need to make sure the dataset is on file when loading the model
  code <- model_obj$code
  ## Strip trailing blank lines/whitespace that cause pharmpy DatasetError
  code <- sub("[\\s\\n]+$", "", code, perl = TRUE)
  if(!is.null(model_obj$dataset) && nrow(model_obj$dataset) > 0) {
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(model_obj$dataset, tmpfile, quote=F, row.names=F)
    code <- stringr::str_replace(
      code,
      "\\$DATA\\s+\\S+",
      paste0("$DATA ", tmpfile)
    )
  }
  model <- pharmr::read_model_from_string(code)
  if(!is.null(model_obj$dataset) && is.null(model$dataset)) {
    model <- pharmr::set_dataset(
      model, path_or_df = tmpfile, datatype = "nonmem"
    )
  }
  model
}
