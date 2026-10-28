# TODO: The functions here should be considered for deprecation since the main fitting function going forward is run_nlme()

#' Fit model using NONMEM or nlmixr2
#'
#' Takes a pharmpy-loaded NONMEM model as input, and returns a pharmpy model
#' results object. So essentially this function is a drop-in replacement for the
#' run_modelfit() function in pharmr/pharmpy.
#'
#' @param model pharmpy model object
#' @param data data.frame with data to fit
#' @param tool either `nonmem` or `nlmixr`
#' @param path path to .rds file to save fit results to
#' @param ... passed onto the tool-specific fitting function
#'
#' @returns TODO
#' 
#' @export
fit_model <- function(model, data, tool = "nonmem", path, ...) {
  if(! "pharmpy.model.external.nonmem.model.Model" %in% class(model)) {
    cli::cli_abort("Needs a pharmpy model object to run.")
  }

  if(tool == "nonmem") {
    fit <- fit_model_nonmem(model, data, ...)
  } else if (tool == "nlmixr") {
    fit <- fit_model_nlmixr(model, data, ...)
  } else {
    cli::cli_abort(paste("Sorry, fit tool", tool, "not supported"))
  }

  ## save fit object to file
  saveRDS(fit$to_dict(), path)

  ## save model to markdown file
  md_path <- stringr::str_replace(path, "\\.rds", ".md")
  save_model_code(model$model_code, md_path)

  return(fit)
}

#' Fit model using nlmixr2
#'
#' @inheritParams fit_model
#'
fit_model_nlmixr <- function(model, data = NULL, ...) {

  if(is.null(data)) { # get from model reference
    data <- model$dataset
  }

  ## Fit
  fit <- pharmr::run_modelfit(
    model,
    tool = "nlmixr"
  )

  return(fit)
}

#' Fit model using NONMEM
#'
#' No longer supported: this relied on a `run_nmfe()` helper that is not part
#' of the package, so it could never complete. Use [run_nlme()] to fit NONMEM
#' models.
#'
#' @inheritParams fit_model
#'
fit_model_nonmem <- function(model, data = NULL, ...) {
  cli::cli_abort(c(
    "{.code fit_model(tool = \"nonmem\")} is no longer supported.",
    "i" = "Use {.fn run_nlme} to fit NONMEM models."
  ))
}
