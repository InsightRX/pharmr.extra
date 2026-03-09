#' Remap a dataset according to a NONMEM \code{$INPUT} block
#'
#' NONMEM matches dataset columns by position, not by name. The \code{$INPUT}
#' block assigns NONMEM names to each positional column and may mark some as
#' \code{DROP}/\code{SKIP}, or use alias notation (\code{A=B}). This function
#' reads the model file, locates the dataset via the \code{$DATA} block, then:
#' \itemize{
#'   \item renames dataset columns to their NONMEM \code{$INPUT} names,
#'   \item removes \code{DROP}/\code{SKIP} columns from both the dataset and
#'         \code{$INPUT},
#'   \item resolves alias notation (\code{A=B}) to simple names.
#' }
#' Dataset columns beyond the number of \code{$INPUT} tokens are dropped
#' (NONMEM ignores them).
#'
#' @param modelfile Path to a NONMEM model file (\code{.mod}/\code{.ctl}).
#'   The dataset path is read from the \code{$DATA} block and resolved relative
#'   to the model file's directory.
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{\code{model_code}}{Updated model code string with the \code{$INPUT}
#'     block cleaned (aliases resolved, DROP/SKIP entries removed).}
#'   \item{\code{dataset}}{Updated data.frame with dropped columns removed and
#'     remaining columns renamed to their NONMEM names.}
#' }
#'
#' @export
remap_nonmem_input <- function(modelfile) {
  if (!file.exists(modelfile)) {
    cli::cli_abort("Model file not found: {modelfile}")
  }
  model_code <- readChar(modelfile, file.info(modelfile)$size)
  model_dir  <- dirname(normalizePath(modelfile))

  mod <- nm_read_model(code = model_code)
  if (is.null(mod[["INPUT"]])) {
    cli::cli_abort("No $INPUT block found in {modelfile}.")
  }
  if (is.null(mod[["DATA"]])) {
    cli::cli_abort("No $DATA block found in {modelfile}.")
  }

  # --- Locate and read dataset from $DATA ----------------------------------
  dataset_file <- .parse_data_filename(mod[["DATA"]])
  if (is.null(dataset_file)) {
    cli::cli_abort("Could not parse dataset filename from $DATA block.")
  }
  # Resolve relative paths with respect to the model file's directory
  if (!file.exists(dataset_file)) {
    dataset_file <- file.path(model_dir, dataset_file)
  }
  if (!file.exists(dataset_file)) {
    cli::cli_abort("Dataset file not found: {dataset_file}")
  }
  dataset <- utils::read.csv(
    dataset_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE
  )

  # --- Parse $INPUT tokens -------------------------------------------------
  # Strip inline comments, collapse lines, remove $INPUT keyword
  input_text <- paste(sub(";.*$", "", mod[["INPUT"]]), collapse = " ")
  input_text <- sub("(?i)^.*?\\$INPUT\\s*", "", input_text, perl = TRUE)
  tokens <- unlist(strsplit(trimws(input_text), "\\s+"))
  tokens <- tokens[nzchar(tokens)]

  n_tok  <- length(tokens)
  n_col  <- ncol(dataset)
  if (n_tok != n_col) {
    cli::cli_warn(c(
      "$INPUT has {n_tok} token{?s} but dataset has {n_col} column{?s}.",
      "i" = "Processing the first {min(n_tok, n_col)} column{?s}."
    ))
  }
  n_process <- min(n_tok, n_col)

  # --- Resolve each token --------------------------------------------------
  nm_names <- character(n_process)
  is_drop  <- logical(n_process)

  for (i in seq_len(n_process)) {
    tok <- tokens[i]
    if (grepl("=", tok, fixed = TRUE)) {
      parts <- strsplit(tok, "=", fixed = TRUE)[[1]]
      lhs   <- toupper(trimws(parts[1]))
      rhs   <- toupper(trimws(parts[2]))
      if (lhs %in% c("DROP", "SKIP") || rhs %in% c("DROP", "SKIP")) {
        is_drop[i]  <- TRUE
        # Keep whichever side is not DROP/SKIP for informational purposes
        nm_names[i] <- if (lhs %in% c("DROP", "SKIP")) rhs else lhs
      } else {
        # True alias (e.g. CONC=DV): use lhs as the NONMEM name
        is_drop[i]  <- FALSE
        nm_names[i] <- lhs
      }
    } else {
      if (toupper(tok) %in% c("DROP", "SKIP")) {
        is_drop[i]  <- TRUE
        nm_names[i] <- NA_character_
      } else {
        is_drop[i]  <- FALSE
        nm_names[i] <- tok
      }
    }
  }

  # --- Build updated dataset -----------------------------------------------
  keep_idx    <- which(!is_drop)
  new_dataset <- dataset[, keep_idx, drop = FALSE]
  colnames(new_dataset) <- nm_names[keep_idx]

  # --- Build clean $INPUT block --------------------------------------------
  new_input_block <- .build_input_block(nm_names[keep_idx])

  # --- Replace $INPUT block in raw model code ------------------------------
  # Match $INPUT up to (but not including) the next $RECORD or end-of-string
  new_code <- sub(
    "(?si)(\\$INPUT[^$]*)(?=\\$[A-Z]|\\z)",
    paste0(new_input_block, "\n"),
    model_code,
    perl = TRUE
  )

  list(
    model_code = new_code,
    dataset    = new_dataset
  )
}

# Extract the dataset filename from the $DATA block lines.
# The filename is the first token that is not $DATA[TA...], IGNORE=, or ACCEPT=.
.parse_data_filename <- function(data_lines) {
  for (line in data_lines) {
    line <- sub(";.*$", "", line)           # strip inline comment
    tokens <- unlist(strsplit(trimws(line), "\\s+"))
    tokens <- tokens[nzchar(tokens)]
    skip <- grepl("^\\$DAT", tokens, ignore.case = TRUE) |
            grepl("^IGNORE=", tokens, ignore.case = TRUE) |
            grepl("^ACCEPT=", tokens, ignore.case = TRUE)
    candidate <- tokens[!skip][1]
    if (!is.na(candidate)) return(candidate)
  }
  NULL
}

.build_input_block <- function(token_names, line_width = 72L) {
  lines   <- character(0)
  current <- "$INPUT"
  for (tok in token_names) {
    candidate <- paste(current, tok)
    if (nchar(candidate) > line_width) {
      lines   <- c(lines, current)
      current <- tok
    } else {
      current <- candidate
    }
  }
  paste(c(lines, current), collapse = "\n")
}
