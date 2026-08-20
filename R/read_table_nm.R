#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R.
#' Function taken from `modelviz` package by Benjamin Guiastrennec.
#' When both \code{skip} and \code{header} are \code{NULL},
#' \code{read_nmtab} will automatically detect the optimal
#' settings to import the tables. When more than one files are
#' provided for a same NONMEM run, they will be combined into
#' a single \code{data.frame}.
#'
#' @param file full file name
#' @param skip number of lines to skip before reading data
#' @param header logical value indicating whether the file contains the names
#' of the variables as its first line
#' @param rm_duplicates logical value indicating whether duplicated columns should be removed
#' @param nonmem_tab logical value indicating to the function whether the file is a
#' table or a nonmem additional output file.
#' @param subproblems keep the simulation subproblems apart. A table written by
#' a `$SIMULATION` record with `SUBPROBLEMS > 1` holds one block of rows per
#' subproblem, each introduced by a repeated `TABLE NO.` header. By default
#' those headers are discarded and the blocks are returned as one undivided
#' concatenation. With `subproblems = TRUE` the table is split on them instead
#' and a 1-based `.subproblem` column is added. Only supported for a single
#' `file` and `nonmem_tab = TRUE`.
#'
#' @returns A \code{data.frame}
#' 
#' @examples
#' \dontrun{
#' data <- read_table_nm(file = '../models/pk/sdtab101')
#' }
#'
#' @export
read_table_nm <- function(
  file = NULL,
  skip = NULL,
  header = NULL,
  rm_duplicates = FALSE,
  nonmem_tab = TRUE,
  subproblems = FALSE
) {

  # Check inputs
  if(is.null(file)) {
    stop('Argument \"file\" required.')
  }

  if(!any(file.exists(file))) {
    stop('No file not found.')
  } else {
    file <- file[file.exists(file)]
  }

  if(isTRUE(subproblems)) {
    if(!nonmem_tab) {
      cli::cli_abort("`subproblems = TRUE` only applies to NONMEM output tables.")
    }
    if(length(file) != 1) {
      cli::cli_abort("`subproblems = TRUE` reads a single table file at a time.")
    }
    tab_file <- read_table_nm_subproblems(file)
    if(rm_duplicates) {
      tab_file <- tab_file[, !duplicated(colnames(tab_file))]
    }
    return(tab_file)
  }

  if(nonmem_tab) {
    # If auto mode required
    if(is.null(skip) & is.null(header)) {
      test    <- readLines(file[1], n = 3)
      skip    <- ifelse(grepl('TABLE NO', test[1]), 1, 0)
      header  <- ifelse(grepl('[a-zA-Z]', test[2]), TRUE, FALSE)
    }

    # Import data
    tab_file <- do.call('cbind', lapply(file, readr::read_table,
                                        skip = skip, col_names = header))

    tab_file <- suppressWarnings(
      as.data.frame(apply(tab_file, MARGIN = 2, FUN = as.numeric))
    )

    # Drop rows with NA (in simtab)
    tab_file <- stats::na.omit(tab_file)

    # Correct bug in the headers
    if(header) {
      colnames(tab_file)[grepl('\n',colnames(tab_file))] <-
        gsub('\n.+', '', colnames(tab_file)[grepl('\n', colnames(tab_file))])
    }

  } else {
    # Search for final results only
    skip     <- max(grep('TABLE NO', readLines(file[1])))

    # Import all files
    tab_file <- do.call('cbind', lapply(file, utils::read.table, skip = skip,
                                        header = FALSE, fill = TRUE, as.is = TRUE))
    colnames(tab_file) <- tab_file[1, ]
    tab_file <- suppressWarnings(as.data.frame(apply(tab_file[-1, ], 2, as.numeric)))
  }

  if(rm_duplicates) {
    tab_file <- tab_file[, !duplicated(colnames(tab_file))]
  }

  tab_file
}

#' Read a NONMEM output table, keeping the simulation subproblems apart
#'
#' `read_table_nm()` skips the leading `TABLE NO.` header and then drops every
#' later one along with the rest of the non-numeric rows, so the subproblem
#' boundaries a `SUBPROBLEMS > 1` run writes into the table are lost. This
#' reader splits on them instead.
#'
#' Rows are classified by whether their first field parses as a number, so it
#' works both for `ONEHEADER` tables (column names written once) and for
#' tables that repeat the column names for every subproblem.
#'
#' @param file full file name of a single NONMEM output table.
#'
#' @returns a `data.frame` with a 1-based integer `.subproblem` column.
#' @noRd
read_table_nm_subproblems <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if(length(lines) == 0) {
    cli::cli_abort("NONMEM output table {.path {file}} is empty.")
  }

  is_marker <- grepl("^\\s*TABLE NO", lines)
  ## A row belongs to the subproblem opened by the last `TABLE NO.` seen.
  ## Tables written without any marker at all are one single subproblem.
  subproblem <- cumsum(is_marker)
  subproblem[subproblem == 0L] <- 1L

  first_token <- sub("\\s.*$", "", trimws(lines))
  is_data <- !is.na(suppressWarnings(as.numeric(first_token)))

  header_lines <- lines[!is_data & !is_marker]
  if(length(header_lines) == 0) {
    cli::cli_abort(
      "No column header found in NONMEM output table {.path {file}}."
    )
  }
  col_names <- strsplit(trimws(header_lines[1]), "[ \t]+")[[1]]

  if(!any(is_data)) {
    cli::cli_abort("No data rows found in NONMEM output table {.path {file}}.")
  }
  ## Not `colClasses = "numeric"`: NONMEM writes `**********` for a value too
  ## wide for its field, which would abort the read rather than come back as
  ## the NA the default reader produces.
  out <- utils::read.table(
    text = paste(lines[is_data], collapse = "\n"),
    header = FALSE,
    as.is = TRUE
  )
  if(ncol(out) != length(col_names)) {
    cli::cli_abort(c(
      "NONMEM output table {.path {file}} has {ncol(out)} column{?s} but \\
       {length(col_names)} column name{?s}.",
      i = "Header read as: {col_names}"
    ))
  }
  colnames(out) <- col_names
  out[] <- lapply(out, function(x) suppressWarnings(as.numeric(x)))
  out[[".subproblem"]] <- as.integer(subproblem[is_data])
  rownames(out) <- NULL
  out
}
