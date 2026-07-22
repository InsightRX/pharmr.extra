#' Create a Pharmpy model object from a NONMEM report file (`.lst`/`.res`)
#'
#' For the case where only a NONMEM report file is available and the `.ext`
#' file (from which pharmpy normally reads final estimates) is missing. Reads
#' the embedded control stream and applies the final parameter estimates parsed
#' from the report's `FINAL PARAMETER ESTIMATE` section.
#'
#' Internally the parsed estimates are written to a synthetic `.ext` file and
#' fed through the existing [create_model_from_file()] path, so pharmpy handles
#' the NONMEM-name to parameter-name mapping.
#'
#' **Precision:** estimates recovered from a report file are rounded to ~3
#' significant figures (see [parse_output()]). Use an `.ext` file with
#' [create_model_from_file()] when full precision is required.
#'
#' @param output_file path to a NONMEM report file (`.lst`, `.res`, ...).
#' @param data optional dataset (filename or `data.frame`) passed to
#'   [create_model_from_file()].
#' @param save_as optional path to write the resulting NONMEM model code to.
#'   Default `NULL` (return the model object only).
#' @param verbose verbose output. Default `TRUE`.
#'
#' @returns a Pharmpy model object.
#'
#' @seealso [parse_output()], [create_model_from_file()]
#'
#' @export
create_model_from_output <- function(
  output_file,
  data = NULL,
  save_as = NULL,
  verbose = TRUE
) {
  if (!inherits(output_file, "character")) {
    cli::cli_abort("`output_file` should be a string.")
  }
  if (!file.exists(output_file)) {
    cli::cli_abort("Report file {output_file} does not exist.")
  }

  parsed <- parse_output(output_file)

  if (verbose) {
    cli::cli_alert_info(
      "Recovering final estimates from {.path {basename(output_file)}}. Values are rounded (~3 sig. figs); use the .ext file for full precision."
    )
  }

  ## Write the control stream and a synthetic .ext into a temp run folder, then
  ## reuse the .ext -> update_parameters() path in create_model_from_file().
  model_id <- basename(tools::file_path_sans_ext(output_file))
  tmp_dir <- tempfile(pattern = paste0(model_id, "_"), tmpdir = tempdir())
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  mod_path <- file.path(tmp_dir, paste0(model_id, ".mod"))
  ext_path <- file.path(tmp_dir, paste0(model_id, ".ext"))
  writeLines(parsed$control_stream, mod_path)
  .write_ext_from_parsed(parsed, ext_path)

  model <- create_model_from_file(
    model_file = mod_path,
    ext_file = ext_path,
    data = data,
    verbose = verbose
  )

  if (!is.null(save_as)) {
    writeLines(as.character(model$code), save_as)
    if (verbose) {
      cli::cli_alert_info("Wrote updated model to {.path {save_as}}.")
    }
  }

  model
}

#' Write a minimal NONMEM `.ext` file from parsed final estimates
#'
#' Produces a single-table `.ext` with the final-estimate marker row
#' (`-1000000000`), which is all pharmpy needs to read final parameter
#' estimates and the OFV. Columns follow the canonical NONMEM ordering:
#' THETA, then lower-triangular OMEGA, then lower-triangular SIGMA.
#'
#' @noRd
.write_ext_from_parsed <- function(parsed, ext_path) {
  th_names <- if (length(parsed$theta) > 0) {
    paste0("THETA", seq_along(parsed$theta))
  } else {
    character(0)
  }

  tri <- function(m, sym) {
    if (is.null(m) || nrow(m) == 0) {
      return(list(names = character(0), vals = numeric(0)))
    }
    n <- nrow(m)
    nm <- character(0)
    v <- numeric(0)
    for (i in seq_len(n)) {
      for (j in seq_len(i)) {
        nm <- c(nm, sprintf("%s(%d,%d)", sym, i, j))
        v <- c(v, m[i, j])
      }
    }
    list(names = nm, vals = v)
  }
  o <- tri(parsed$omega, "OMEGA")
  s <- tri(parsed$sigma, "SIGMA")

  names_all <- c(th_names, o$names, s$names)
  vals_all <- c(parsed$theta, o$vals, s$vals)
  ofv <- if (is.na(parsed$ofv)) NaN else parsed$ofv

  fmt <- function(x) formatC(x, format = "E", digits = 5, width = 13)
  header1 <- paste0(
    "TABLE NO.     1: First Order Conditional Estimation with Interaction: ",
    "Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 ",
    "Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0"
  )
  header2 <- paste(c(" ITERATION", names_all, "OBJ"), collapse = "  ")
  final_row <- paste(
    c(
      sprintf("%12s", "-1000000000"),
      vapply(vals_all, fmt, character(1)),
      sprintf("%.15f", ofv)
    ),
    collapse = "  "
  )
  writeLines(c(header1, header2, final_row), ext_path)
}
