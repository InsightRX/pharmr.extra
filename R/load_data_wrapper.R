#' Wrapper to load data from either filename or data.frame
#'
#' @inheritParams create_model
#'
#' @returns data.frame or NULL
load_data_wrapper <- function(data) {
  if(inherits(data, "character")) {
    if(file.exists(data)) {
      dataset <- read.csv(file = data)
    } else {
      cli::cli_abort("`data` file does not exist.")
    }
  } else {
    dataset <- data
  }
  dataset
}
