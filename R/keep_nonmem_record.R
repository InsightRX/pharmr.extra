## Which files a run folder's record is made of. Each pattern is matched
## against a file's path *relative to the run folder*, so a rule can name the
## subfolder a file has to sit in and not only its name.

#' Files worth keeping from a [run_sim()] run folder
#'
#' The control stream and the listing of every regimen / replicate: the
#' `$SIMULATION` record with its seeds, and the NM-TRAN warnings, are only
#' there.
#' @noRd
keep_pattern_nonmem_sim <- "(^|/)run\\.(mod|lst)$"

#' Files worth keeping from a [run_nlme()] fit folder
#'
#' The control stream and listing, plus the estimation output NONMEM writes
#' beside them, the streamed console files (`console = FALSE`) and the final
#' estimates. `stderr` is part of the record because an nmfe run that falls
#' over writes its reason there and nowhere else. `.ext` and friends are
#' matched by extension rather than as `run.ext`, because a PsN run names them
#' after its own stem (`psn.ext`). Left behind: the dataset, the output tables
#' (`sdtab`, `patab`), the iteration/debug files (`.phi`, `.grd`, `.xml`, ...)
#' and NONMEM's build files.
#' @noRd
keep_pattern_nonmem_fit <- paste0(
  "(^|/)(run\\.(mod|lst)|final\\.mod|stdout|stderr)$",
  "|\\.(ext|shk|cor|cov)$"
)

#' Files worth keeping from a [call_pharmpy_tool()] run folder
#'
#' A search writes one folder per candidate fit; keeping all of them would
#' defeat the point. Kept instead: the base fit at the folder's root (written
#' by the [run_nlme()] call that produced `results`), the tool's own result
#' summaries, the `final_<tool>.mod` this package writes, and the candidate the
#' search settled on (`<tool>N/models/final`, or `models/sim` for
#' `tool = "simulation"`). The candidate's files are matched by extension
#' because Pharmpy names them after the model, not after `run.mod`.
#' @noRd
keep_pattern_pharmpy_tool <- paste0(
  "^(run\\.(mod|lst)|final\\.mod|stdout|stderr)$|^[^/]*\\.(ext|shk|cor|cov)$",
  "|^final_[^/]*\\.mod$",
  "|(^|/)results\\.(csv|json)$",
  "|(^|/)models/(final|sim)/[^/]*\\.(mod|lst|ext|shk|cor|cov)$"
)

#' Keep a record of what NONMEM ran, then remove the run folder
#'
#' The `keep` argument of [run_sim()], [run_nlme()] and [call_pharmpy_tool()].
#' Every file under `staging` whose relative path matches `pattern` is copied
#' to `keep` at that same relative path, and `staging` is then removed with
#' everything else in it: the datasets, output tables and NONMEM's build files.
#' Registered as an exit handler by its callers, so a run that aborts still
#' keeps its listing.
#'
#' @param staging the run folder, `file.path(path, id)` as [run_sim()] lays it
#' out. Need not exist: a run that aborted before creating it leaves nothing
#' to keep and nothing to remove.
#' @param keep destination folder, created as needed. `NULL` does nothing.
#' @param created was the run folder created by this run? A folder that was
#' already there when the run started is only removed once the run has written
#' a file matching `pattern` into it: without one, nothing says the folder
#' holds this run rather than an earlier one's results or unrelated files.
#' @param pattern regular expression the record is selected by, matched against
#' each file's path relative to `staging` (`"regimen_1/run.lst"`), so a rule can
#' name the subfolder a file has to sit in. Defaults to [run_sim()]'s record.
#' Hidden files and folders are never part of a record.
#'
#' @returns the absolute `keep` path, invisibly (`NULL` when `keep` is `NULL`).
#' @noRd
keep_nonmem_record <- function(
  staging,
  keep,
  created = TRUE,
  pattern = keep_pattern_nonmem_sim
) {
  if(is.null(keep)) return(invisible(NULL))
  keep <- validate_keep_folder(keep, staging = staging)
  dir.create(keep, recursive = TRUE, showWarnings = FALSE)
  if(!dir.exists(staging)) return(invisible(keep))

  ## Matched on the relative path rather than through `list.files(pattern = )`,
  ## which only ever sees a file's name: a search's record is "the model under
  ## models/final", which the name alone cannot express.
  files <- list.files(staging, recursive = TRUE)
  files <- files[grepl(pattern, files)]
  ## Nothing of this run's in a folder this run did not create: leave it alone
  ## rather than remove someone else's files.
  if(!created && length(files) == 0) return(invisible(keep))
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

#' Check a caller's `keep` argument
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
      i = "The run folder {.path {staging_abs}} is removed once its record \\
           has been copied to {.arg keep}."
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
