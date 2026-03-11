#' Get declared variables in Pharmpy model
#' 
#' @param model Pharmpy model object
#' 
#' @export
#' 
#' @returns character vector of declared variables
#'
get_declared_variables <- function(model) {
  lhs_vars <- as.character(model$internals$old_statements$lhs_symbols) |>
    stringr::str_replace_all("[\\{\\}]", "") |>
    stringr::str_split(",\\s?") |>
    unlist()
  ## remove internal vars:
  internal_vars <- c("Y", "F", "W")
  if(any(internal_vars %in% lhs_vars)) {
    lhs_vars <- lhs_vars[! lhs_vars %in% internal_vars]
  }
  ## remove any vars with brackets
  lhs_vars <- lhs_vars[!stringr::str_detect(lhs_vars, "\\(")]
  lhs_vars
}