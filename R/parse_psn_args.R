#' Parse tool options specified in YAML into PsN commandline args
#'
#' @param options list of options. Logical arguments should be specified
#' as TRUE/FALSE.
#'
#' @export
#' 
parse_psn_args <- function(options) {
  options$id <- NULL
  options$tool <- NULL
  if(is.null(options) || length(options) == 0) {
    return(NULL)
  }
  ## split in logical and epxlicit options
  logical_options <- list()
  for(key in names(options)) {
    if(class(options[[key]]) == "logical") {
      logical_options[[key]] <- options[[key]]
      options[[key]] <- NULL
    }
  }
  args <- c(paste0("--", names(options), "=", options))
  if(length(logical_options) > 0) {
    args <- c(args, paste0("--", names(logical_options)))
  }
  args
}
