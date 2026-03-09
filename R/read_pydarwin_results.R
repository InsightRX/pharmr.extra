#' Read pyDarwin search results
#'
#' Parses the `results.csv` produced by a pyDarwin search and returns it as
#' a tidy data frame, sorted by fitness (best model first).
#'
#' @param output_dir Path to the pyDarwin output directory (the directory that
#'   contains `results.csv`).
#' @param verbose Emit a message if no results file is found?
#'
#' @return A data frame with one row per evaluated model, or `NULL` if
#'   `results.csv` does not exist in `output_dir`.
#'
#' @export
read_pydarwin_results <- function(output_dir, verbose = TRUE) {
  results_path <- file.path(output_dir, "results.csv")
  if (!file.exists(results_path)) {
    if (verbose) {
      cli::cli_alert_warning("No results.csv found in {output_dir}")
    }
    return(NULL)
  }
  res <- readr::read_csv(results_path, show_col_types = FALSE)

  ## sort by fitness column if present (lower = better in pyDarwin)
  fitness_col <- intersect(c("fitness", "Fitness", "OFV"), names(res))
  if (length(fitness_col) > 0) {
    res <- dplyr::arrange(res, .data[[fitness_col[1]]])
  }

  res
}
