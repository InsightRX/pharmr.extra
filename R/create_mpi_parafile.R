#' Write a NONMEM MPI parafile
#'
#' Writes a NONMEM MPI parafile for within-model parallelization. The
#' parafile defines `[nodes]` workers; the actual node count can be
#' overridden on the nmfe command line via `[nodes]=N`.
#'
#' Layout matches the standard `mpilinux8.pnm` shipped with NONMEM:
#' `TRANSFER_TYPE=1` (MPI), `PARSE_TYPE=4`, and `mpirun` invoking `./nonmem`
#' on the master node with worker nodes inheriting the MPI launch context.
#' Requires MPI (e.g. OpenMPI) to be installed and `mpirun` on the PATH at
#' run time. FPI mode is not currently supported.
#'
#' @param path directory in which to write the parafile.
#' @param threads number of worker nodes (default `[nodes]` value).
#' @param filename file name for the parafile. Default `parafile.pnm`.
#'
#' @returns the absolute path to the written parafile.
#' @keywords internal
create_mpi_parafile <- function(path, threads, filename = "parafile.pnm") {
  if(!dir.exists(path)) {
    cli::cli_abort("Directory {.path {path}} does not exist.")
  }
  if(!is.numeric(threads) || threads < 1 || threads != as.integer(threads)) {
    cli::cli_abort("`threads` must be a positive integer.")
  }
  parafile_path <- file.path(normalizePath(path, mustWork = TRUE), filename)
  contents <- c(
    "$GENERAL",
    "NODES=[nodes] PARSE_TYPE=4 PARAPRINT=0 TRANSFER_TYPE=1",
    "",
    "$COMMANDS",
    '1: mpirun -wdir "$PWD" -n 1 ./nonmem  $*',
    '2-[nodes]: -wdir "$PWD/worker{#-1}" -n 1 ./nonmem',
    "",
    "$DIRECTORIES",
    "1:NONE",
    "2-[nodes]:worker{#-1}",
    "",
    "$DEFAULTS",
    paste0("[nodes]=", as.integer(threads))
  )
  writeLines(contents, parafile_path)
  parafile_path
}
