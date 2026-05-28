#' Ensure the default `.ext` file exists in a NONMEM run folder
#'
#' NONMEM's `$ESTIMATION FILE=...` option redirects the iteration output to
#' a non-default filename (e.g. PsN-style runs write `psn.ext`). pharmpy's
#' result reader derives the `.ext` path from the model stem (`run.mod` →
#' `run.ext`) and will fail silently when that file is missing or empty.
#'
#' This helper is called after the fit completes. If `<stem>.ext` is missing
#' or zero-bytes, and exactly one other non-empty `.ext` file is present in
#' the fit folder, that file is copied to `<stem>.ext`. When zero or more
#' than one candidate is found, nothing is copied.
#'
#' @param fit_folder path to the run folder.
#' @param model_file model filename (e.g. `"run.mod"`); the stem determines
#'   the expected `.ext` name.
#' @param verbose emit a `cli` message describing the action. Default `TRUE`.
#'
#' @returns Invisibly `TRUE` if a file was copied, `FALSE` otherwise.
#'
#' @export
ensure_default_ext_file <- function(fit_folder, model_file, verbose = TRUE) {
  stem <- tools::file_path_sans_ext(basename(model_file))
  default_ext <- file.path(fit_folder, paste0(stem, ".ext"))

  if (file.exists(default_ext) && file.info(default_ext)$size > 0) {
    return(invisible(FALSE))
  }

  candidates <- list.files(
    fit_folder, pattern = "\\.ext$", full.names = TRUE
  )
  candidates <- setdiff(candidates, default_ext)
  candidates <- candidates[file.info(candidates)$size > 0]

  if (length(candidates) == 0) {
    return(invisible(FALSE))
  }
  if (length(candidates) > 1) {
    if (verbose) {
      cli::cli_alert_warning(
        "Multiple non-default .ext files in {.path {fit_folder}}: {.path {basename(candidates)}}. Not copying any to {.path {basename(default_ext)}}."
      )
    }
    return(invisible(FALSE))
  }

  file.copy(candidates, default_ext, overwrite = TRUE)
  if (verbose) {
    cli::cli_alert_info(
      "Copied {.path {basename(candidates)}} to {.path {basename(default_ext)}} so pharmpy can read the fit."
    )
  }
  invisible(TRUE)
}
