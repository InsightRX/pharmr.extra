#' Coerce files, model code, and more to pharmpy model objects
#'
#' `as_pharmpy_model()` turns an existing object, such as a NONMEM model file or
#' code, into a Pharmpy model.
#'
#' @param model Either a Pharmpy model object, a path to a model file, a string
#'   containing NONMEM model code, or a pharmpy model stored using
#'   [export_pharmpy_model()]. Model files, strings, and stored models will be
#'   coerced to a Pharmpy model.
#'
#' @returns A Pharmpy model.
#' @export
as_pharmpy_model <- function(model) {
  # Read model from file or string:
  if (is.character(model)) {
    if (fs::is_file(model) & fs::path_ext(model) == "rds") {
      model <- import_pharmpy_model(model)
    } else if (fs::is_file(model)  & fs::path_ext(model) %in% c("mod", "ctl", "nmctl")) {
      model <- pharmr::read_model(model)
    } else {
      model <- pharmr::read_model_from_string(model)
    }
  }
  
  # Return existing or read model object if successful:
  if (inherits(model, "pharmpy.model.model.Model")) {
    return(model)
  }
  
  cli::cli_abort(
    "Could not read model into Pharmpy. Please check supplied model object, file, or code."
  )
}
