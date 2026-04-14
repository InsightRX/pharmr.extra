#' Extract $TABLE records from a Pharmpy NONMEM model object
#'
#' @inheritParams run_nlme
#'
#' @returns character vector of $TABLE blocks, or NULL if no tables exist
#'
#' @keywords internal
get_table_records <- function(model) {
  if(!inherits(model, "pharmpy.model.external.nonmem.model.Model")) {
    return(NULL)
  }
  obj <- nm_read_model(code = model$code)
  if(is.null(obj$TABLE)) {
    return(NULL)
  }
  obj$TABLE
}

#' Restore $TABLE records in a Pharmpy NONMEM model object
#'
#' Replaces all $TABLE blocks in the model code with the saved table records,
#' then re-parses the model. This is used to work around pharmpy bugs that
#' corrupt $TABLE records during estimation step updates.
#'
#' @inheritParams run_nlme
#' @param saved_tables character vector of $TABLE lines as returned by
#'   \code{get_table_records()}
#'
#' @returns updated pharmpy model object
#'
#' @keywords internal
restore_table_records <- function(model, saved_tables) {
  data <- model$dataset
  model_code <- model$code
  # Remove all existing $TABLE blocks
  code_without_tables <- remove_table_sections(model_code)
  # Rebuild the saved $TABLE blocks as a single string
  table_str <- paste0(saved_tables, collapse = "\n")
  # Append saved tables to the code
  code_with_tables <- paste0(
    trimws(code_without_tables, which = "right"),
    "\n",
    table_str,
    "\n"
  )
  model <- pharmr::read_model_from_string(code = code_with_tables)
  if(!is.null(data)) {
    model <- pharmr::set_dataset(model, path_or_df = data)
  }
  model
}
