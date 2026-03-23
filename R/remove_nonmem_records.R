#' Function to remove specific NONMEM records from model file
#'
#' @param text NONMEM model code
#' 
#' @returns NONMEM model code
#' 
remove_nonmem_records <- function(text, short_name = "EST") {
  pattern <- paste0("\\$", short_name, "[^$]+")
  result <- gsub(pattern, "", text, perl = TRUE)
  
  # Handle case where $TABLE is the last section
  pattern <- paste0("\\$", short_name, ".*$")
  result <- gsub(pattern, "", result, perl = TRUE)
  
  # Clean up any extra newlines that might be left
  result <- gsub("\n{3,}", "\n\n", result)
  
  # Trim any trailing whitespace
  result <- trimws(result, which = "right")
  return(result)
}
