#' Validate the specified model, ensure it's valid Pharmpy model
#'
#' @param model TODO
validate_model <- function(
  model
) {
  if(inherits(model, "pharmpy.model.model.Model")) {
    tool <- get_tool_from_model(model)
    if(tool != "nonmem") {
      cli::cli_abort("Currently only NONMEM is supported.")
    }
  } else if(inherits(model, "character")) {
    tool <- "nonmem"
    if(file.exists(model)) { ## specified as file?
      model <- create_model_from_file(model)
    } else { ## specified as code?
      tmpfile <- tempfile(pattern = "mod_")
      on.exit(unlink(tmpfile), add = TRUE)
      writeLines(paste0(model, collapse = "\n"), tmpfile)
      model <- create_model_from_file(tmpfile)
    }
  } else {
    cli::cli_abort("`model` should either be model code or a pharmpy model object")
  }
  model
}
