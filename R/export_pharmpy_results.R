#' Export a pharmpy model object
#' 
#' Currently, pharmr/pharmpy is not able to export results objects from grid 
#' searches or model fits. This function aims to work around that until it does.
#' It essentially saves the model information separately, and upon reloading
#' it reconstructs the models in the results object. 
#' 
#' The saved object is not a Pharmpy object, but it has most of the same
#' entries so can be used as a drop-in.
#' 
#' @param results Pharmpy results object to save
#' @param file JSON file to save to
#'
#' @export
#' 
export_pharmpy_results <- function(results, file) {
  if(inherits(results, "pharmpy.tools.modelsearch.tool.ModelSearchResults")) {
    out <- list(
      final_model = list(
        code = results$final_model$code,
        dataset = results$final_model$dataset,
        datainfo = results$final_model$datainfo
      ),
      models = list(),
      summary_models = results$summary_models,
      summary_tool = results$summary_tool,
      summary_errors = results$summary_errors,
      final_results = results$final_results
    )
    for(i in seq_along(results$models)) {
      out$models[[i]] <- list(
        code = results$models[[i]]$code,
        dataset = results$models[[i]]$dataset,
        datainfo = results$models[[i]]$datainfo
      ) 
    }
    class(out) <- "pharmpy.tools.modelsearch.tool.ModelSearchResults"
  } else if(inherits(results, "pharmpy.workflows.results.ModelfitResults")) {
    out <- results$to_dict()
    attr(out, "model") <- list(
      code = attr(results, "model")$code,
      dataset = attr(results, "model")$dataset,
      datainfo = attr(results, "model")$datainfo
    )
    attr(out, "final_model") <- list(
      code = attr(results, "final_model")$code,
      dataset = attr(results, "final_model")$dataset,
      datainfo = attr(results, "final_model")$datainfo
    )
    attr(out, "tables") <- attr(results, "tables")
    class(out) <- "pharmpy.workflows.results.ModelfitResults"
  } else {
    cli::cli_abort("Object class not recognized, cannot export.")
  }
  saveRDS(out, file)
}

#' Import pharmpy model results object
#' 
#' @param file RDS file to load model object from
#' 
#' @export
#' 
import_pharmpy_results <- function(file) {
  results <- readRDS(file)
  if(inherits(results, "pharmpy.tools.modelsearch.tool.ModelSearchResults")) {
    results$final_model <- create_pharmpy_model_from_list(results$final_model)
    for(i in seq_along(results$models)) {
      results$models[[i]] <- create_pharmpy_model_from_list(results$models[[i]])
    }
  } else if(inherits(results, "pharmpy.workflows.results.ModelfitResults")) {
    attr(results, "model") <- create_pharmpy_model_from_list(
      attr(results, "model")
    )
    attr(results, "final_model") <- create_pharmpy_model_from_list(
      attr(results, "final_model")
    )
  } else if (inherits(results, "list") && all(c("code", "dataset", "datainfo") %in% names(results))) {
    cli::cli_abort("Object looks like a Pharmpy model object. Please use `import_pharmpy_model()` to import Pharmpy models.")
  } else {
    cli::cli_abort("Object class not recognized, cannot import.")
  }
  results
}
