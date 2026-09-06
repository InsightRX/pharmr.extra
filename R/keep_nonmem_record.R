#' Keep a record of what NONMEM ran, then remove the run folder
#'
#' The `keep` argument of [run_sim()]. Every `run.mod` and `run.lst` found
#' under `staging` is copied to `keep` at the same relative path, and `staging`
#' is then removed with everything else in it: the datasets, output tables and
#' NONMEM's build files. Registered as an exit handler by [run_sim()], so a run
#' that aborts still keeps its listing.
#'
#' @param staging the run folder, `file.path(path, id)` as [run_sim()] lays it
#' out. Need not exist: a run that aborted before creating it leaves nothing
#' to keep and nothing to remove.
#' @param keep destination folder, created as needed. `NULL` does nothing.
#'
#' @returns the absolute `keep` path, invisibly (`NULL` when `keep` is `NULL`).
#' @noRd
keep_nonmem_record <- function(staging, keep) {
  if(is.null(keep)) return(invisible(NULL))
  keep <- validate_keep_folder(keep, staging = staging)
  dir.create(keep, recursive = TRUE, showWarnings = FALSE)
  if(!dir.exists(staging)) return(invisible(keep))

  files <- list.files(staging, pattern = "^run\\.(mod|lst)$", recursive = TRUE)
  copied <- vapply(files, function(f) {
    to <- file.path(keep, f)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(staging, f), to, overwrite = TRUE)
  }, logical(1))
  if(!all(copied)) {
    ## Removing the run folder would then destroy the only copy of the record.
    cli::cli_warn(c(
      "Could not copy {sum(!copied)} file{?s} to {.path {keep}}; \\
       the run folder {.path {staging}} is left in place.",
      x = "{.file {files[!copied]}}"
    ))
    return(invisible(keep))
  }
  unlink(staging, recursive = TRUE)
  invisible(keep)
}

#' Check `run_sim()`'s `keep` argument
#'
#' `keep` must be a single non-empty string, and must not be the run folder or
#' lie inside it: [keep_nonmem_record()] removes the run folder once the record
#' has been copied out, which would take the copy with it. With `base` given
#' (the folder the run id was resolved against), the run folder must also be a
#' proper subfolder of it, so an `id` such as `"."` cannot turn the removal
#' into removing the working directory.
#'
#' @param keep the caller's `keep`.
#' @param staging the run folder.
#' @param base the folder `staging` was resolved against, or `NULL` to skip
#' that check.
#'
#' @returns the absolute `keep` path.
#' @noRd
validate_keep_folder <- function(keep, staging, base = NULL) {
  if(!is.character(keep) || length(keep) != 1 || is.na(keep) || !nzchar(keep)) {
    cli::cli_abort(
      "{.arg keep} must be a single non-empty string naming a folder, or NULL."
    )
  }
  keep_abs <- absolute_path(keep)
  staging_abs <- absolute_path(staging)
  ## Checked first: with `id = "."` the run folder is the working directory,
  ## and "keep lies inside the run folder" would be the less helpful message.
  if(!is.null(base)) {
    base_abs <- absolute_path(base)
    if(!startsWith(staging_abs, paste0(base_abs, "/"))) {
      cli::cli_abort(c(
        "{.arg keep} removes the run folder after the run, and \\
         {.path {staging_abs}} is not a subfolder of {.path {base_abs}}.",
        i = "Give {.arg id} a folder name of its own."
      ))
    }
  }
  if(keep_abs == staging_abs || startsWith(keep_abs, paste0(staging_abs, "/"))) {
    cli::cli_abort(c(
      "{.arg keep} must not be the run folder or lie inside it.",
      x = "{.arg keep} resolves to {.path {keep_abs}}.",
      i = "The run folder {.path {staging_abs}} is removed once its \\
           {.file run.mod} and {.file run.lst} have been copied to {.arg keep}."
    ))
  }
  keep_abs
}

#' Absolute path, symlinks resolved, for a location that need not exist yet
#'
#' `normalizePath()` returns a path that does not exist untouched, so a `keep`
#' folder that is yet to be created would compare differently from the run
#' folder that already exists under a symlinked parent (macOS's `/var` is
#' one). Resolve the deepest existing ancestor and put the remainder back.
#'
#' @param p a path, relative to the working directory unless absolute.
#'
#' @returns a single absolute path, without a trailing slash.
#' @noRd
absolute_path <- function(p) {
  p <- as.character(fs::path_norm(fs::path_abs(p)))
  head <- p
  tail <- character(0)
  while(!file.exists(head)) {
    parent <- as.character(fs::path_dir(head))
    if(parent == head) break
    tail <- c(as.character(fs::path_file(head)), tail)
    head <- parent
  }
  as.character(fs::path_norm(fs::path_join(
    c(normalizePath(head, mustWork = FALSE), tail)
  )))
}
