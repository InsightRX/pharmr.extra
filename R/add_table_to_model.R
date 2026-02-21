#' Add new $TABLE record to output variables
#'
#' @param model pharmpy model object
#' @param variables character vector with variable names
#' @param firstonly add `FIRSTONLY` parameter to $TABLE record
#' @param file path to file, e.g. `sdtab`
#'
#' @returns TODO
#'
#' @export
add_table_to_model <- function(
    model,
    variables,
    firstonly = FALSE,
    file
) {
  check_nm_table_variables(model, variables)
  tool <- get_tool_from_model(model)
  if(tool == "nonmem") {
    existing_tables <- get_tables_in_model_code(model$code)
    data <- model$dataset
    if(file %in% existing_tables) {
      warning("Table file already in a $TABLE record in model.")
      return(model)
    }
    if(is.null(variables) || length(variables) == 0) {
      warning("No variables to add to $TABLE, skipping.")
      return(model)
    }
    table_code <- paste0(
      "\n$TABLE\n",
      paste0(c(" ", variables), collapse = " "),
      ifelse(firstonly, "\n  FIRSTONLY", ""),
      "\n  NOAPPEND NOPRINT",
      "\n  FILE=", file,
      "\n\n"
    )
    model <- pharmr::read_model_from_string(
      code = paste0(model$code, table_code)
    )
    if(!is.null(data)) {
      model <- pharmr::set_dataset(model, data)
    }
  } else {
    ## Adding tables can only be done for NONMEM datasets
  }
  return(model)
}

#' Check NONMEM table variables
#'
#' @inheritParams add_table_to_model
#' @param throw_error should throw error? If FALSE, will return vector of 
#' invalid variables.
#'
#' @returns `NULL` if all variables are valid, otherwise an error (or return
#' all invalid variables if `throw_error` is FALSE.
#' 
check_nm_table_variables <- function(model, variables, throw_error = TRUE) {
  is_data_variable <- variables %in% model$datainfo$names
  is_lhs_variable <- variables %in% get_lhs_variables(model)
  is_defined_pk_parameter <- variables %in% get_defined_pk_parameters(model)
  is_eta <- variables %in% get_nm_table_etas(model)
  is_nm_reserved_variable <- variables %in% nonmem_reserved_variables
  
  is_valid <- is_data_variable | 
    is_lhs_variable | 
    is_defined_pk_parameter | 
    is_eta | 
    is_nm_reserved_variable
  if (all(is_valid)) return()
  
  invalid_variables <- variables[!is_valid]
  if(throw_error) {
    cli::cli_abort(c(
      paste0(
        "$TABLE variables must be any of the following: ",
        "A data variable, a left-hand side variable in the model, ",
        "or a reserved NONMEM $TABLE variable."
      ),
      "x" = "{.field {invalid_variables}} {?is/are} not {?a/} valid variable{?s}.",
      "i" = "Did you make a typo?"
    ))
  }
  invalid_variables
}

get_lhs_variables <- function(model) {
  vapply(
    reticulate::iterate(model$statements$lhs_symbols),
    function(.x) reticulate::py_to_r(reticulate::py_eval("str")(.x)),
    character(1)
  )
}

get_nm_table_etas <- function(model) {
  eta_names <- model$random_variables$iiv$names
  n_etas <- length(eta_names)
  eta_numeric_labels <- c(paste0("ETA", 1:n_etas), paste0("ETA(", 1:n_etas, ")"))
  # Requires NONMEM 7.4:
  eta_abbr_labels <- paste0(
    "ETA(", stringr::str_extract(model$random_variables$iiv$names, "(?<=_).+"), ")"
  )
  
  c(eta_numeric_labels, eta_abbr_labels)
}
