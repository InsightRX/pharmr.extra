#' Resolve an output filename against the run's `path`
#'
#' Fit artefacts (`<id>.rds`, `<id>_fit_summary.txt`,
#' `<id>_fit_parameters.csv`) used to be written with bare relative names, so
#' they landed in `getwd()` even when the caller passed an explicit `path`.
#' This helper anchors relative names to `path` instead. Absolute paths are
#' left untouched — POSIX roots, `~`-prefixed paths, Windows drive letters and
#' UNC shares — so an explicit `save_fit = "/some/where/fit.rds"` still goes
#' exactly where asked.
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
  ## Anything the OS would not resolve against the working directory is left
  ## alone: POSIX roots (`/x`), home-relative (`~/x`), Windows drive-qualified
  ## (`C:\x`, `C:/x`), UNC shares (`\\server\share`, `//server/share`) and
  ## drive-relative roots (`\x`). Done with a regex rather than
  ## `normalizePath()` because the latter resolves against the *current*
  ## working directory, which is exactly the behaviour being avoided here.
  is_absolute <- grepl("^(~|[/\\\\]|[A-Za-z]:[/\\\\])", file)
  if(is_absolute) {
    return(file)
  }
  if(!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  file.path(path, file)
}
