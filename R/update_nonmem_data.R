#' Update the dataset path in NONMEM model code
#'
#' Replaces the dataset path in the first `$DATA` record with a new path,
#' while preserving every other element: any options on the `$DATA` line
#' (`IGNORE=@`, `ACCEPT=(...)`, `REWIND`, `RECORDS=N`, ...), any inline
#' comment on that line, and all other records in the model code.
#'
#' The match is case-insensitive and also accepts the abbreviated `$DAT`
#' record name. Dataset paths in the original record may be unquoted or
#' wrapped in single or double quotes. If the new `path` contains
#' whitespace, it is wrapped in single quotes so NONMEM parses it as one
#' token.
#'
#' @param code NONMEM model code. Either a single character string (possibly
#' with embedded newlines) or a character vector of lines.
#' @param path path to the new dataset.
#'
#' @returns The updated model code, in the same shape as `code`: a single
#' string if `code` was a string, a character vector otherwise.
#'
#' @examples
#' code <- "$PROB Test\n$DATA old.csv IGNORE=@ ACCEPT=(DV.GT.0)\n$INPUT ID TIME DV"
#' cat(update_nonmem_data(code, "new.csv"))
#'
#' @export
update_nonmem_data <- function(code, path) {
  if (!is.character(code) || length(code) == 0) {
    cli::cli_abort("`code` must be a non-empty character vector or string.")
  }
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    cli::cli_abort("`path` must be a single non-empty character string.")
  }

  was_string <- length(code) == 1
  if (was_string) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  } else {
    lines <- code
  }

  ## Find the first $DATA record, ignoring any text inside comments.
  data_idx <- NULL
  for (i in seq_along(lines)) {
    stripped <- sub(";.*$", "", lines[[i]])
    if (grepl("^\\s*\\$DAT", stripped, ignore.case = TRUE, perl = TRUE)) {
      data_idx <- i
      break
    }
  }
  if (is.null(data_idx)) {
    cli::cli_abort("No $DATA record found in model code.")
  }

  line <- lines[[data_idx]]

  ## Split off any inline comment so the comment is preserved verbatim.
  comment <- ""
  semi <- regexpr(";", line, fixed = TRUE)
  if (semi > 0) {
    comment <- substr(line, semi, nchar(line))
    line <- substr(line, 1L, semi - 1L)
  }

  ## Parse [whitespace]$DAT(A)[whitespace](path)(rest)
  ## where `path` can be a double-quoted, single-quoted, or bare token.
  m <- regmatches(
    line,
    regexec(
      "^(\\s*\\$DAT[A-Za-z]*\\s+)(\"[^\"]*\"|'[^']*'|\\S+)(.*)$",
      line,
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1]]

  if (length(m) == 0) {
    cli::cli_abort(
      "Could not parse $DATA record on line {data_idx}: {.val {lines[[data_idx]]}}"
    )
  }

  prefix <- m[[2]]
  rest   <- m[[4]]

  ## Quote the new path if it contains whitespace so NONMEM treats it as
  ## a single token.
  new_path <- if (grepl("\\s", path, perl = TRUE)) paste0("'", path, "'") else path

  lines[[data_idx]] <- paste0(prefix, new_path, rest, comment)

  if (was_string) {
    paste(lines, collapse = "\n")
  } else {
    lines
  }
}
