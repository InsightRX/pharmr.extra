#' Validate the specified model, ensure it's valid Pharmpy model
#'
#' @inheritParams run_nlme
#' 
validate_model <- function(
  model,
  data = NULL
) {
  if(inherits(model, "pharmpy.model.model.Model")) {
    tool <- get_tool_from_model(model)
    if(! tool %in% c("nonmem", "nlmixr")) {
      cli::cli_abort("Unsupported model engine: {tool}.")
    }
  } else if(inherits(model, "character")) {
    tool <- "nonmem"
    if(file.exists(model)) { ## specified as file?
      model <- create_model_from_file(model)
    } else { ## specified as code?
      tmpfile <- tempfile(pattern = "mod_", fileext = ".mod")
      on.exit(unlink(tmpfile), add = TRUE)
      writeLines(paste0(model, collapse = "\n"), tmpfile)
      model <- create_model_from_file(tmpfile, data = data)
    }
  } else {
    cli::cli_abort("`model` should either be model code or a pharmpy model object")
  }
  model
}
