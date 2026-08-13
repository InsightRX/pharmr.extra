#' Resolve an output filename against the run's `path`
#'
#' Fit artefacts (`<id>.rds`, `<id>_fit_summary.txt`,
#' `<id>_fit_parameters.csv`) used to be written with bare relative names, so
#' they landed in `getwd()` even when the caller passed an explicit `path`.
#' This helper anchors relative names to `path` instead. Absolute paths (and
#' `~`-prefixed paths) are left untouched, so an explicit
#' `save_fit = "/some/where/fit.rds"` still goes exactly where asked.
#'
#' @param file filename, possibly relative.
#' @param path folder to resolve relative filenames against. `NULL` or `NA`
#' falls back to the working directory.
#'
#' @returns character path.
#' @keywords internal
resolve_output_file <- function(file, path = NULL) {
  if(is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(file)
  }
  ## `normalizePath(mustWork = FALSE)` gives an absolute path for "~/..." too,
  ## so comparing against the input detects both absolute forms.
  is_absolute <- grepl("^(/|~|[A-Za-z]:[\\\\/])", file)
  if(is_absolute) {
    return(file)
  }
  if(!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  file.path(path, file)
}
