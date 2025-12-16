#' Export a pharmpy model object. 
#' 
#' Pharmpy model objects loaded in R cannot be stored and
#' retrieved currently using native Pharmpy code. This is a 
#' (hopefully temporary) wrapper function to export the model
#' and dataset to an RDS file.
#' 
#' @param model Pharmpy model object to save
#' @param file RDS file to save to
#'
#' @export
#' 
export_pharmpy_model <- function(
  model,
  file
) {
  if(! all(c("code", "dataset", "datainfo") %in% names(model))) {
    cli::cli_abort("The object you are trying to export is not a Pharmpy model object.")
  }
  saveRDS(
    list(
      code = model$code,
      dataset = model$dataset,
      datainfo = model$datainfo
    ),
    file
  )
}

#' Import a pharmpy model stored using `export_pharmpy_model`
#' 
#' @param file RDS file to load model object from
#' 
#' @export
#' 
import_pharmpy_model <- function (
  file
) {
  model_obj <- readRDS(file)
  if(! all(c("code", "dataset", "datainfo") %in% names(model_obj))) {
    if(inherits(model_obj, "pharmpy.tools.modelsearch.tool.ModelSearchResults") || inherits(model_obj, "pharmpy.tools.modelsearch.tool.ModelFitResults")) {
      cli::cli_abort("The object you are trying to import is not a Pharmpy model object but a Pharmpy results object. Please use `import_pharmpy_results() to load it.")
    } else {
      cli::cli_abort("The object you are trying to import is not a Pharmpy model object.")
    }
  }
  model <- create_pharmpy_model_from_list(model_obj)
  model
}
