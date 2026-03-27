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
#' \code{$INPUT} (e.g. \code{WT=WEIGHT}, where \code{WT} is the NONMEM
#' internal name and \code{WEIGHT} is the data-file column) are handled
#' correctly.
#'
#' @param model Path to a NONMEM \code{.mod}/\code{.ctl} file, NONMEM model
#'   code as a single string, or a Pharmpy NONMEM model object.
#' @param data Optional. A data.frame or path to a CSV file whose column names
#'   are used to populate \code{data_col}. Columns are matched to \code{$INPUT}
#'   entries positionally (same order). When omitted the filename from
#'   \code{$DATA} is tried automatically; if that file cannot be found,
#'   \code{data_col} falls back to \code{nonmem_name}.
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
get_required_input_variables <- function(model, data = NULL, include_reserved_nonmem = TRUE) {
  model_dir <- NULL
  if (inherits(model, "pharmpy.model.model.Model")) {
    nm <- nm_read_model(code = pharmr::get_model_code(model))
  } else if (is.character(model) && length(model) == 1 && file.exists(model)) {
    model_dir <- dirname(model)
    nm <- nm_read_model(modelfile = model)
  } else if (is.character(model)) {
    nm <- nm_read_model(code = model)
  } else {
    cli::cli_abort("`model` must be a file path, NONMEM code string, or Pharmpy model object.")
  }

  # Parse $INPUT record
  input_df <- .parse_nm_input(nm$INPUT)

  # Resolve data columns: use explicit `data` argument, or fall back to the
  # file referenced in $DATA, or finally default to nonmem_name.
  data_cols <- .resolve_data_cols(data, nm$DATA, model_dir, nrow(input_df))
  input_df$data_col <- if (!is.null(data_cols)) data_cols else input_df$nonmem_name

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
#' @returns A data frame with columns \code{nonmem_name} and \code{dropped}.
#' @keywords internal
.parse_nm_input <- function(input_lines) {
  # Strip inline comments per line, then join lines and drop the $INPUT header token
  input_lines <- sub(";.*$", "", input_lines)
  text <- paste(input_lines, collapse = " ")
  text <- sub("^\\$INPUT\\s*", "", text, ignore.case = TRUE)

  tokens <- unlist(strsplit(trimws(text), "\\s+"))
  tokens <- tokens[nzchar(tokens)]

  rows <- lapply(tokens, function(tok) {
    if (grepl("=", tok, fixed = TRUE)) {
      parts <- strsplit(tok, "=", fixed = TRUE)[[1]]
      lhs <- parts[1]  # NONMEM internal name (label used in model code)
      rhs <- parts[2]  # data file column label, or DROP
      if (toupper(rhs) == "DROP" || toupper(lhs) == "DROP") {
        list(nonmem_name = if (toupper(lhs) == "DROP") NA_character_ else lhs, dropped = TRUE)
      } else {
        list(nonmem_name = lhs, dropped = FALSE)
      }
    } else if (toupper(tok) == "DROP") {
      list(nonmem_name = NA_character_, dropped = TRUE)
    } else {
      list(nonmem_name = tok, dropped = FALSE)
    }
  })

  data.frame(
    nonmem_name = vapply(rows, `[[`, character(1), "nonmem_name"),
    dropped     = vapply(rows, `[[`, logical(1),   "dropped"),
    stringsAsFactors = FALSE
  )
}

#' Resolve data column names from an explicit data argument or $DATA filename
#'
#' Returns a character vector of column names (length \code{n_input}), or
#' \code{NULL} if no usable data source was found.
#'
#' @param data User-supplied \code{data} argument (data.frame, file path, or NULL).
#' @param data_lines Lines of the \code{$DATA} record from the parsed model.
#' @param model_dir Directory of the model file, used to resolve relative paths.
#' @param n_input Number of \code{$INPUT} entries expected.
#' @keywords internal
.resolve_data_cols <- function(data, data_lines, model_dir, n_input) {
  # Helper: read column names from a CSV file path, returning NULL on failure.
  .cols_from_file <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(
      names(read.csv(path, nrows = 0, check.names = FALSE)),
      error = function(e) NULL
    )
  }

  # Helper: validate length and trim/warn as needed. Returns NULL only if data
  # has fewer columns than $INPUT (positional match would be wrong).
  .check_length <- function(cols, source_desc) {
    if (is.null(cols)) return(NULL)
    if (length(cols) < n_input) {
      cli::cli_warn(
        "Data source {source_desc} has only {length(cols)} column(s) but \\
         $INPUT has {n_input} entries — cannot match positionally. \\
         Using nonmem_name for data_col."
      )
      return(NULL)
    }
    if (length(cols) > n_input) {
      cols <- cols[seq_len(n_input)]
    }
    cols
  }

  # 1. Explicit data argument
  if (!is.null(data)) {
    if (is.data.frame(data)) {
      return(.check_length(names(data), "`data`"))
    } else if (is.character(data) && length(data) == 1) {
      cols <- .cols_from_file(data)
      if (is.null(cols)) cli::cli_abort("File not found: {.path {data}}")
      return(.check_length(cols, paste0("'", data, "'")))
    } else {
      cli::cli_abort("`data` must be a data.frame or a path to a CSV file.")
    }
  }

  # 2. $DATA filename from model code
  if (!is.null(data_lines) && length(data_lines) > 0) {
    data_line <- paste(data_lines, collapse = " ")
    data_line <- gsub(";[^\n]*", "", data_line)  # strip comments
    # First token after $DATA is the filename
    tokens <- strsplit(trimws(sub("^\\$DATA\\s*", "", data_line, ignore.case = TRUE)), "\\s+")[[1]]
    data_file <- tokens[1]
    if (!is.na(data_file) && nzchar(data_file)) {
      # Try as-is, then relative to model directory
      path <- if (file.exists(data_file)) {
        data_file
      } else if (!is.null(model_dir) && file.exists(file.path(model_dir, data_file))) {
        file.path(model_dir, data_file)
      } else {
        cli::cli_warn(
          "$DATA file {.path {data_file}} not found — using nonmem_name for data_col. \\
           Pass the `data` argument explicitly to supply column names."
        )
        return(NULL)
      }
      cols <- .cols_from_file(path)
      if (!is.null(cols)) return(.check_length(cols, paste0("$DATA file '", data_file, "'")))
    }
  }

  NULL  # fall back to nonmem_name in the caller
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
