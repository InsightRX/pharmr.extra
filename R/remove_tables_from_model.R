#' Remove all $TABLE records from a model
#'
#' @param model pharmpy model object
#' @param file remove only a specific table defined as FILE=<file>. `file` can
#' also specify only the start of a filename, e.g. `patab`
#'
#' @returns TODO
#' 
#' @export
remove_tables_from_model <- function(
  model,
  file = NULL
) {
  tool <- get_tool_from_model(model)
  if(tool == "nonmem") {

    ## if there's no tables to begin with, then just return unchanged
    existing_tables <- get_tables_in_model_code(model$code)
    if(length(existing_tables) == 0) {
      return(model)
    }

    ## Reread model without tables
    code_without_tables <- remove_table_sections(model$code, file = file)
    temp_mod <- tempfile(pattern = "tmp_mod_", fileext = '.mod')
    writeLines(code_without_tables, temp_mod)
    model <- create_model_from_file(model_file = temp_mod, data = model$dataset)
  } else {
    ## Removing tables can only be done for NONMEM datasets
  }
  return(model)
}

#' Remove $DATA from a NONMEM model
#'
#' @param text model code
#' @returns character string
remove_data_section <- function(text) {
  pattern <- "\\$DATA[^$]+"
  result <- gsub(pattern, "", text, perl = TRUE)
  result
}

#' Function to remove all $TABLE sections
#'
#' @param text TODO
#' @param file TODO
remove_table_sections <- function(text, file = NULL) {
  if(!is.null(file)) {
    if(length(file) != 1) {
      cli::cli_abort("`file` should be of length 1")
    }
    pattern <- paste0("\\$TABLE[^$]+FILE=", file)
  } else {
    pattern <- "\\$TABLE[^$]+"
  }
  result <- gsub(pattern, "", text, perl = TRUE)

  # Handle case where $TABLE is the last section
  if(!is.null(file)) {
    result <- gsub("\\$TABLE.*FILE=.*$", "", result, perl = TRUE)
  } else {
    result <- gsub("\\$TABLE.*$", "", result, perl = TRUE)
  }

  # Clean up any extra newlines that might be left
  result <- gsub("\n{3,}", "\n\n", result)

  # Trim any trailing whitespace
  result <- trimws(result, which = "right")
  return(result)
}
