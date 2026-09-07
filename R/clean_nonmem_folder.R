#' Remove temporary files from NONMEM run
#' 
#' @param path path to NONMEM run folder
#' 
#' @returns TODO
#' 
#' @export
clean_nonmem_folder <- function(path) {
  ## `all.files`: `.modeldb` and `.pharmpy` below are hidden, and `dir()`
  ## leaves those out by default.
  files <- dir(path, all.files = TRUE)
  blacklist <- c(
    ## Pharmpy's scratch, written when the fit is dispatched through it. Both
    ## are folders, hence the `recursive` unlink below.
    ".modeldb",
    ".pharmpy",
    "compile.lnk",
    "FCON",
    "FDATA",
    "FDATA.csv",
    "FMSG",
    "FORIG",
    "FREPL",
    "FREPORT",
    "FSIZES",
    "FSTREAM",
    "FSUBS",
    "FSUBS2",
    "FSUBS.f90",
    "gfortran.txt",
    "INTER",
    "LINKC.LNK",
    "LINK.LNK",
    "nmpathlist.txt",
    "nmprd4p.mod",
    "nonmem",
    "PRSIZES.f90",
    "parafile.pnm"
  )
  rm_files <- file.path(path, intersect(blacklist, files))
  unlink(rm_files, recursive = TRUE)
}
