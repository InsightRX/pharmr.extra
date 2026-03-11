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
  if(! inherits(path_or_df, "character")) {
    new_dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(path_or_df, new_dataset_file, quote = F, row.names = F)
  } else {
    new_dataset_file <- path_or_df
  }
  model_code <- model$code
  model_path <- tempfile(fileext = ".mod")
  model_code <- change_nonmem_dataset(
    model_code,
    new_dataset_file
  )
  writeLines(model_code, model_path)
  model <- model |>
    pharmr::read_model(path = model_path)
  model
}
