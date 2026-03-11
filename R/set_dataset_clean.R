#' Drop-in replacement for pharmr::set_dataset_clean that avoids
#' any type-conversion issues.
#' 
#' @param path_or_df path to data file or data.frame / tibble
#' 
#' @returns Pharmpy model object
#' 
#' @export
#' 
set_dataset_clean <- function(model, path_or_df) {
  model_code <- model$code
  if(! inherits(path_or_df, "character")) {
    df <- as.data.frame(path_or_df)
  } else {
    df <- read.csv(path_or_df)
  }
  ## Reorder columns to match $INPUT record to avoid pharmpy positional
  ## column misassignment when data has fewer columns than $INPUT
  input_line <- grep("^\\$INPUT", strsplit(model_code, "\n")[[1]], value = TRUE)[1]
  if (!is.na(input_line)) {
    input_cols <- strsplit(trimws(sub("^\\$INPUT\\s*", "", input_line)), "\\s+")[[1]]
    present <- input_cols[input_cols %in% names(df)]
    extra <- names(df)[!names(df) %in% input_cols]
    df <- df[, c(present, extra), drop = FALSE]
  }
  new_dataset_file <- tempfile(pattern = "data", fileext = ".csv")
  write.csv(df, new_dataset_file, quote = F, row.names = F)
  model_path <- tempfile(fileext = ".mod")
  model_code <- change_nonmem_dataset(model_code, new_dataset_file)
  writeLines(model_code, model_path)
  pharmr::read_model(path = model_path)
}
