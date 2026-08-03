## Helpers for the FORMAT / IDFORMAT options of a NONMEM $TABLE record.
##
## Background (verified against NONMEM 7.5.1):
## - `FORMAT` applies to *every* column of the table, so a fixed-point spec such
##   as `sF9.0` rounds concentrations, times and parameters to whole numbers.
##   `IDFORMAT` applies to the ID column only and leaves the rest at NONMEM's
##   default, which is what we want when only the ID column needs more digits.
## - Both options are carried over to all *subsequent* $TABLE records of the
##   control stream, so setting FORMAT once corrupts every later table too.
## - Each column is written into a field of `width(FORMAT) + 1` characters
##   (12 by default, since NONMEM's default FORMAT is `s1PE11.4`). If the ID
##   value does not fit in `width(IDFORMAT)`, NONMEM writes `*********`; if
##   `width(IDFORMAT) > width(FORMAT)`, NONMEM dies at table-writing time with
##   an unhelpful "Fortran runtime error: End of record".

## Field width of a NONMEM $TABLE format spec, e.g. "sF11.0" -> 11,
## "s1PE11.4" -> 11. Returns NA_integer_ if the spec cannot be parsed.
nm_table_format_width <- function(format) {
  if(is.null(format) || is.na(format)) return(NA_integer_)
  width <- stringr::str_match(
    format,
    "(?i)[EFGD]\\s*(\\d+)"   ## the width follows the edit descriptor letter
  )[, 2]
  suppressWarnings(as.integer(width))
}

#' Validate the FORMAT / IDFORMAT options of a $TABLE record
#'
#' @param id_format `IDFORMAT` spec, or `NULL`
#' @param format `FORMAT` spec, or `NULL` for the NONMEM default (`s1PE11.4`)
#'
#' @returns `NULL`, invisibly; called for the error it may throw.
#'
#' @keywords internal
check_table_formats <- function(id_format, format = NULL) {
  is_spec <- function(x) is.null(x) || (is.character(x) && length(x) == 1)
  if(!is_spec(id_format)) {
    cli::cli_abort("{.arg id_format} must be a single string or {.code NULL}.")
  }
  if(!is_spec(format)) {
    cli::cli_abort("{.arg format} must be a single string or {.code NULL}.")
  }
  if(is.null(id_format)) return(invisible(NULL))

  id_width <- nm_table_format_width(id_format)
  ## NONMEM's default $TABLE format is s1PE11.4, i.e. 11 characters wide.
  format_width <- if(is.null(format)) 11L else nm_table_format_width(format)
  if(is.na(id_width) || is.na(format_width)) return(invisible(NULL))

  if(id_width > format_width) {
    cli::cli_abort(c(
      "{.arg id_format} ({.val {id_format}}) is wider than the column width of {.arg format} ({format_width} characters).",
      "x" = "NONMEM aborts with {.emph Fortran runtime error: End of record} when writing the table.",
      "i" = "Use an {.arg id_format} of at most {format_width} characters, or widen {.arg format}."
    ))
  }
  invisible(NULL)
}
