#' Detect any errors in the output from NONMEM.
#' 
#' @param output text outputted from NONMEM as a vector of lines.
#' 
#' @export
check_errors_nm_output <- function(output) {
  potential_errors <- c("DATA ERROR", "AN ERROR WAS FOUND")
  matches <- unlist(
    lapply(
      potential_errors, 
      function(x) { 
        grep(x, output) 
      }
    )
  )
  if(any(matches)) {
    idx_first <- min(matches, na.rm=TRUE)
    error_message <- output[idx_first:length(output)]
    stop(error_message)
  }
}
