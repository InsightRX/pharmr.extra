#' Get required input variables for a NONMEM model
#'
#' Parses a NONMEM model and determines which variables from \code{$INPUT} are
#' required to create a new input dataset. Variables are classified as:
#' \itemize{
#'   \item \code{"reserved"} -- standard NONMEM data items with intrinsic
#'     meaning (ID, TIME, DV, AMT, EVID, etc.)
#'   \item \code{"dose_variable"} -- columns referenced on the right-hand side
#'     of a dose-timing parameter assignment (\code{D1}–\code{D9},
#'     \code{ALAG1}–\code{ALAG9}, \code{F1}–\code{F9}, \code{R1}–\code{R9})
#'     in \code{$PK} (e.g. \code{D1 = DUR} or \code{D1 = DUR * 24}).
#'     These must be specified per dose event, not per subject.
#'   \item \code{"used_covariate"} -- non-reserved columns explicitly referenced
#'     in the model code (\code{$PK}, \code{$DES}, \code{$ERROR}, \code{$PRED})
#'     but not classified as a dose variable
#'   \item \code{"unused_covariate"} -- columns present in \code{$INPUT} but
#'     never referenced in model code
#'   \item \code{"dropped"} -- columns marked \code{DROP} in \code{$INPUT}
#' }
#'
#' \code{"reserved"}, \code{"dose_variable"}, and \code{"used_covariate"}
#' columns are all considered required for simulation. Renames in
#' \code{$INPUT} (e.g. \code{WT=WEIGHT}, where \code{WT} is the data-file
#' column and \code{WEIGHT} is the NONMEM internal name) are handled correctly.
#'
#' @param model Path to a NONMEM \code{.mod}/\code{.ctl} file, or NONMEM model
#'   code as a single string.
#' @param include_reserved_nonmem Logical. If \code{TRUE} (default), reserved
#'   NONMEM variables are included in the returned data frame. Set to
#'   \code{FALSE} to return only covariate-type variables (useful when you only
#'   need to know which subject-level covariates to include).
#'
#' @returns A \code{data.frame} with columns:
#'   \describe{
#'     \item{nonmem_name}{Name used inside NONMEM model code.}
#'     \item{data_col}{Corresponding column name in the data file.}
#'     \item{type}{Classification: \code{"reserved"}, \code{"dose_variable"},
#'       \code{"used_covariate"}, \code{"unused_covariate"}, or
#'       \code{"dropped"}.}
#'     \item{required}{\code{TRUE} if the column must be present in a new input
#'       dataset.}
#'   }
#'
#' @export
get_required_input_variables <- function(model, include_reserved_nonmem = TRUE) {
  if (is.character(model) && length(model) == 1 && file.exists(model)) {
    nm <- nm_read_model(modelfile = model)
  } else if (is.character(model)) {
    nm <- nm_read_model(code = model)
  } else {
    cli::cli_abort("`model` must be a file path or NONMEM code string.")
  }

  # Parse $INPUT record
  input_df <- .parse_nm_input(nm$INPUT)

  # Collect model equation code; exclude $TABLE, $DATA, etc.
  equation_blocks <- c("PK", "DES", "ERROR", "PRED")
  present <- equation_blocks[equation_blocks %in% names(nm)]
  model_code <- paste(unlist(nm[present]), collapse = "\n")
  # Strip inline comments
  model_code <- gsub(";[^\n]*", "", model_code)

  # Determine which $DATA record uses IGNORE=C (C column is then required)
  ignore_c <- FALSE
  if (!is.null(nm$DATA)) {
    data_line <- paste(nm$DATA, collapse = " ")
    ignore_c <- grepl("IGNORE\\s*=\\s*C\\b", data_line, ignore.case = TRUE)
  }

  # Standard NONMEM reserved data item names
  reserved <- c(
    "ID", "L1", "L2", "DV", "MDV", "EVID", "AMT", "TIME",
    "DATE", "DAT1", "DAT2", "DAT3", "RATE", "ADDL", "II", "SS",
    "CMT", "PCMT", "CALL", "CONT"
  )
  if (ignore_c) reserved <- c(reserved, "C")

  # Detect dose-timing variables: any input column referenced on the RHS of a
  # D<n>, ALAG<n>, F<n>, or R<n> assignment in $PK, whether a simple
  # assignment (`D1 = DUR`) or an expression (`D1 = DUR * 24`).
  dose_vars <- .find_dose_variables(model_code, input_df$nonmem_name)

  # Check if each variable's NONMEM name appears in the model code
  input_df$used_in_code <- mapply(
    function(nm_name, is_dropped) {
      if (is.na(nm_name) || is_dropped) return(FALSE)
      grepl(paste0("\\b", nm_name, "\\b"), model_code)
    },
    input_df$nonmem_name,
    input_df$dropped
  )

  input_df$type <- dplyr::case_when(
    input_df$dropped                              ~ "dropped",
    input_df$nonmem_name %in% reserved            ~ "reserved",
    input_df$nonmem_name %in% dose_vars           ~ "dose_variable",
    input_df$used_in_code                         ~ "used_covariate",
    TRUE                                          ~ "unused_covariate"
  )
  input_df$required <- input_df$type %in% c("reserved", "dose_variable", "used_covariate")

  out <- input_df[, c("nonmem_name", "data_col", "type", "required")]
  if (!include_reserved_nonmem) {
    out <- out[out$type != "reserved", ]
  }
  out
}

#' Parse $INPUT record lines into a data frame
#'
#' @param input_lines Character vector of lines from the \code{$INPUT} record.
#' @returns A data frame with columns \code{nonmem_name}, \code{data_col},
#'   \code{dropped}.
#' @keywords internal
.parse_nm_input <- function(input_lines) {
  # Join lines, drop the $INPUT header token and strip comments
  text <- paste(input_lines, collapse = " ")
  text <- sub("^\\$INPUT\\s*", "", text, ignore.case = TRUE)
  text <- gsub(";[^\n]*", " ", text)

  tokens <- unlist(strsplit(trimws(text), "\\s+"))
  tokens <- tokens[nzchar(tokens)]

  rows <- lapply(tokens, function(tok) {
    if (grepl("=", tok, fixed = TRUE)) {
      parts <- strsplit(tok, "=", fixed = TRUE)[[1]]
      lhs <- parts[1]  # data file column label
      rhs <- parts[2]  # NONMEM internal name, or DROP
      if (toupper(rhs) == "DROP") {
        list(nonmem_name = lhs, data_col = lhs, dropped = TRUE)
      } else if (toupper(lhs) == "DROP") {
        # Edge case: DROP=something (anonymous dropped column)
        list(nonmem_name = rhs, data_col = NA_character_, dropped = TRUE)
      } else {
        list(nonmem_name = rhs, data_col = lhs, dropped = FALSE)
      }
    } else if (toupper(tok) == "DROP") {
      # Positional, anonymous drop (no label)
      list(nonmem_name = NA_character_, data_col = NA_character_, dropped = TRUE)
    } else {
      list(nonmem_name = tok, data_col = tok, dropped = FALSE)
    }
  })

  data.frame(
    nonmem_name = vapply(rows, `[[`, character(1), "nonmem_name"),
    data_col    = vapply(rows, `[[`, character(1), "data_col"),
    dropped     = vapply(rows, `[[`, logical(1),   "dropped"),
    stringsAsFactors = FALSE
  )
}

#' Find input variables referenced in dose-timing parameter assignments
#'
#' Scans comment-stripped model code for lines where a dose-timing parameter
#' (\code{D<n>}, \code{ALAG<n>}, \code{F<n>}, \code{R<n>}) is on the left-hand
#' side of an assignment, then extracts every identifier on the right-hand side
#' that is also an \code{$INPUT} column. This covers both simple assignments
#' (\code{D1 = DUR}) and expressions (\code{D1 = DUR * 24}).
#'
#' @param model_code Comment-stripped model code string.
#' @param input_names Character vector of NONMEM names from \code{$INPUT}.
#' @keywords internal
.find_dose_variables <- function(model_code, input_names) {
  lhs_pattern <- "^\\s*(?:D|ALAG|F|R)\\d+\\s*=(.+)$"
  lines <- strsplit(model_code, "\n")[[1]]
  found <- character(0)
  for (line in lines) {
    m <- regmatches(line, regexec(lhs_pattern, line, perl = TRUE))[[1]]
    if (length(m) == 2) {
      rhs_ids <- regmatches(m[2], gregexpr("[A-Za-z][A-Za-z0-9_]*", m[2]))[[1]]
      found <- c(found, intersect(rhs_ids, input_names))
    }
  }
  unique(found)
}
