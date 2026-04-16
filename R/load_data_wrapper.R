#' Wrapper to load data from either filename or data.frame
#'
#' @inheritParams create_model
#'
#' @returns data.frame or NULL
load_data_wrapper <- function(data) {
  if(inherits(data, "character")) {
    if(file.exists(data)) {
      dataset <- read.csv(file = data)
    } else {
      cli::cli_abort("`data` file does not exist.")
    }
  } else {
    dataset <- data
  }
  unquote_column_names(dataset)
}

#' Strip surrounding quote characters from data.frame column names
#'
#' Pharmpy rejects column names that are not valid Python identifiers, and
#' NONMEM misinterprets CSV headers that start with a quote character. This
#' helper removes a single pair of matching surrounding single or double
#' quotes from each column name.
#'
#' @param data data.frame (or NULL / non-data.frame, returned unchanged)
#'
#' @returns data.frame with sanitised column names, or the input unchanged
unquote_column_names <- function(data) {
  if (is.null(data) || !inherits(data, "data.frame")) return(data)
  nm <- names(data)
  new_nm <- sub("^([\"\\'])(.*)\\1$", "\\2", nm, perl = TRUE)
  if (!identical(nm, new_nm)) {
    names(data) <- new_nm
  }
  data
}
