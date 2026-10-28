#' Helper function to determine nmfe location from various sources
#' The order is as follows:
#'
#' 1. argument specified by user
#' 2. check pharmpy config
#' 3. throw error, force user to specify
#'
#' @param nmfe full path to the nmfe script. If `NULL` (default), the location
#'   is read from the Pharmpy configuration.
#' @param verbose verbose output?
#'
get_nmfe_location <- function(
  nmfe = NULL,
  verbose = FALSE
) {
  if(!is.null(nmfe)) {
    if(verbose) cli::cli_alert_info("Using user-specified NONMEM version at {nmfe}")
  } else {
    pharmpy_conf <- get_pharmpy_conf()
    nm_path <- pharmpy_conf$pharmpy.plugins.nonmem$default_nonmem_path
    if(is.null(nm_path)) {
      cli::cli_abort("Pharmpy is not configured to run NONMEM.")
    }
    if(!file.exists(nm_path)) {
      cli::cli_abort("NONMEM path configured in Pharmpy is not a valid folder. Please reconfigure Pharmpy")
    }
    nmfe_file <- detect_nmfe_version(nm_path)
    nmfe <- file.path(nm_path, "run", nmfe_file)
    if(verbose) cli::cli_alert_info("Using Pharmpy-configured NONMEM version at {nmfe}")
  }
  if(is.null(nmfe) || !file.exists(nmfe)) {
    cli::cli_abort("Could not determine path to NONMEM nmfe file, please specify manually with `nmfe` argument or reconfigure Pharmpy.")
  }
  nmfe
}

#' get nmfe file name from a NONMEM installation folder
#'
#' @param nm_path path to the NONMEM installation folder (the folder that
#'   contains the `run/` subfolder).
#'
#' @returns file name of the first `nmfe<NN>` script found in `run/`, or `NA`
#'   if none is found.
detect_nmfe_version <- function(nm_path) {
  files <- dir(
    file.path(nm_path, "run"),
    pattern = "nmfe\\d\\d"
  )
  files[1]
}