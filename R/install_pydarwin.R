#' Install pyDarwin into the r-reticulate Python environment
#'
#' @param envname Name of the Python virtual environment to install into.
#'   Defaults to `"r-reticulate"` (the same environment used by pharmpy).
#'
#' @export
install_pydarwin <- function(envname = "r-reticulate") {
  reticulate::py_install(
    "pyDarwin-Certara",
    envname = envname,
    pip = TRUE,
    pip_options = "--pre"
  )
}
