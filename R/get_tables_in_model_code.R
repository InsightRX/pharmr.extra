#' extract FILE names from $TABLE using simple regex.
#' For some reason the tables are not (yet?) available in pharmpy
#' 
#' @param code a character string with NONMEM model code
#' 
#' @returns TODO
#' 
#' @export
get_tables_in_model_code <- function(code) {
  if(length(code) == 0) return(character(0))
  txt <- stringr::str_replace_all(code, "\\n", " ")
  tables <- stringr::str_match_all(
    txt,
    "\\$TABLE\\s+(?:(?!\\$).)*?FILE=([^\\s]+)"
  )[[1]][,2]
  tables
}
