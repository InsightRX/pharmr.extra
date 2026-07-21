#' Parse a NONMEM report file (`.lst`/`.res`)
#'
#' Extracts the embedded control stream and the final parameter estimates
#' (THETA vector, OMEGA and SIGMA covariance matrices, and the objective
#' function value) from a NONMEM report/output file. This is useful when only
#' the report file is available and the `.ext` file (which pharmpy normally
#' reads final estimates from) is missing.
#'
#' **Precision:** the `FINAL PARAMETER ESTIMATE` block in a `.lst` is rounded to
#' roughly three significant figures, so estimates recovered this way are lower
#' precision than those in the corresponding `.ext` file. Use the `.ext` file
#' (via [create_model_from_file()]) when full precision is required.
#'
#' Standard errors and covariance-step output are not parsed.
#'
#' **Limitations:** OMEGA/SIGMA matrices are read from the single column block
#' NONMEM prints for up to six random effects; wider matrices (which NONMEM
#' splits across multiple column blocks) are not yet supported.
#'
#' @param lst_file path to a NONMEM report file (`.lst`, `.res`, ...).
#' @param code character string with the report file contents (alternative to
#'   `lst_file`).
#'
#' @returns a list with elements `control_stream` (character), `theta` (numeric
#'   vector), `omega` (numeric matrix or `NULL`), `sigma` (numeric matrix or
#'   `NULL`), and `ofv` (numeric scalar or `NA`).
#'
#' @export
parse_lst <- function(lst_file = NULL, code = NULL) {
  if (is.null(code)) {
    if (is.null(lst_file)) {
      cli::cli_abort(
        "Please specify a NONMEM report file (`lst_file`) or its contents (`code`)."
      )
    }
    if (!file.exists(lst_file)) {
      cli::cli_abort("NONMEM report file ({lst_file}) not found.")
    }
    code <- readChar(lst_file, file.info(lst_file)$size)
  }
  lines <- stringr::str_split(code, "\\r?\\n")[[1]]

  list(
    control_stream = .extract_control_stream(lines),
    theta = .parse_lst_theta(lines),
    omega = .parse_lst_matrix(lines, "OMEGA - COV MATRIX"),
    sigma = .parse_lst_matrix(lines, "SIGMA - COV MATRIX"),
    ofv = .parse_lst_ofv(lines)
  )
}

#' Extract numeric tokens from a line of NONMEM report output
#'
#' `.........` (used by NONMEM for structural zeros / fixed off-diagonals) is
#' mapped to `0` before extraction.
#'
#' @noRd
.nm_numbers <- function(x) {
  x <- gsub("\\.{3,}", " 0 ", x)
  m <- stringr::str_extract_all(
    x, "[-+]?[0-9]*\\.?[0-9]+[eE][-+]?[0-9]+|[-+]?[0-9]*\\.?[0-9]+"
  )[[1]]
  as.numeric(m)
}

#' Slice the control-stream echo out of a NONMEM report file
#'
#' NONMEM echoes the control stream from the first record (`$...`) up to the
#' `NM-TRAN MESSAGES` banner.
#'
#' @noRd
.extract_control_stream <- function(lines) {
  start <- which(stringr::str_detect(lines, "^\\s*\\$"))[1]
  if (is.na(start)) {
    cli::cli_abort(
      "No NONMEM control-stream records ($...) found in report file."
    )
  }
  end_marker <- which(stringr::str_detect(lines, "NM-TRAN MESSAGES"))[1]
  end <- if (is.na(end_marker)) length(lines) else end_marker - 1L
  stream <- lines[start:end]
  while (length(stream) > 0 &&
         !stringr::str_detect(stream[length(stream)], "\\S")) {
    stream <- stream[-length(stream)]
  }
  paste(stream, collapse = "\n")
}

#' Return the lines of the `FINAL PARAMETER ESTIMATE` section
#'
#' Bounded by the `STANDARD ERROR OF ESTIMATE` banner (if present) so the
#' repeated headers / correlation matrices there are excluded.
#'
#' Multi-step estimation (e.g. SAEM followed by IMP) prints one section per
#' step; the **last** one holds the final estimates, matching the last table
#' pharmpy reads from the `.ext` file.
#'
#' @noRd
.final_est_region <- function(lines) {
  starts <- which(stringr::str_detect(lines, "FINAL PARAMETER ESTIMATE"))
  if (length(starts) == 0) {
    cli::cli_abort(
      "No 'FINAL PARAMETER ESTIMATE' section found in report file; the run may not have completed."
    )
  }
  start <- starts[length(starts)]
  se <- which(stringr::str_detect(lines, "STANDARD ERROR OF ESTIMATE"))
  se <- se[se > start][1]
  end <- if (is.na(se)) length(lines) else se - 1L
  lines[start:end]
}

# Lines that start a new sub-section within the final-estimate region.
.lst_section_break <- "MATRIX FOR RANDOM|VECTOR OF FIXED|STANDARD ERROR|\\*{6,}"

#' Parse the final THETA vector from a NONMEM report file
#'
#' @noRd
.parse_lst_theta <- function(lines) {
  fp <- .final_est_region(lines)
  hdr <- which(stringr::str_detect(fp, "THETA - VECTOR OF FIXED EFFECTS"))[1]
  if (is.na(hdr)) return(numeric(0))
  rest <- fp[(hdr + 1L):length(fp)]
  stop_at <- which(stringr::str_detect(rest, .lst_section_break))[1]
  if (!is.na(stop_at)) rest <- rest[seq_len(stop_at - 1L)]
  # Value lines carry E-notation; the `TH 1  TH 2` label line does not.
  val_lines <- rest[stringr::str_detect(rest, "[0-9]\\.[0-9]*[eE]")]
  unlist(lapply(val_lines, .nm_numbers), use.names = FALSE)
}

#' Parse a final OMEGA/SIGMA covariance matrix from a NONMEM report file
#'
#' Matrix rows are the `+`-prefixed value lines that follow the given header.
#' The returned matrix is symmetric (lower triangle mirrored).
#'
#' @noRd
.parse_lst_matrix <- function(lines, header) {
  fp <- .final_est_region(lines)
  hdr <- which(stringr::str_detect(fp, header))[1]
  if (is.na(hdr)) return(NULL)
  rest <- fp[(hdr + 1L):length(fp)]
  stop_at <- which(stringr::str_detect(rest, .lst_section_break))[1]
  if (!is.na(stop_at)) rest <- rest[seq_len(stop_at - 1L)]
  row_lines <- rest[stringr::str_detect(rest, "^\\s*\\+")]
  if (length(row_lines) == 0) return(NULL)
  rows <- lapply(row_lines, .nm_numbers)
  n <- length(rows)
  m <- matrix(0, n, n)
  for (i in seq_len(n)) {
    ri <- rows[[i]]
    for (j in seq_along(ri)) {
      m[i, j] <- ri[j]
      m[j, i] <- ri[j]
    }
  }
  m
}

#' Parse the objective function value from a NONMEM report file
#'
#' Prefers the full-precision `OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT` line.
#'
#' @noRd
.parse_lst_ofv <- function(lines) {
  ln <- lines[stringr::str_detect(lines, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT")]
  if (length(ln) == 0) {
    ln <- lines[stringr::str_detect(lines, "MINIMUM VALUE OF OBJECTIVE FUNCTION")]
  }
  if (length(ln) == 0) return(NA_real_)
  v <- .nm_numbers(ln[length(ln)])
  if (length(v) == 0) NA_real_ else v[length(v)]
}
