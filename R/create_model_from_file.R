#' Create a Pharmpy model object from a model file and dataset (optional)
#' 
#' @param model_file the model file (.mod) to read.
#' @param data the filename of the dataset (or an actual data.frame)
#' 
#' @returns a Pharmpy model object
#'
#' @export
create_model_from_file <- function(
  model_file,
  data = NULL
) {
  ## Checks
  if(! inherits(model_file, "character")) {
    cli::cli_abort("Model file should be a string.")
  }
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file {model_file} does not exist")
  }
  if(inherits(data, "data.frame") || inherits(data, "tibble")) {
    ## do nothing
  } else if (inherits(data, "character")) {
    data_file <- data
    if (!file.exists(data_file)) {
      cli::cli_abort("Data file {data_file} does not exist")
    }
    data <- read.csv(data_file)
  }
  
  ## Create Pharmpy object
  tryCatch({
    model_code <- readLines(model_file)
    model <- pharmr::read_model_from_string(paste(model_code, collapse = "\n"))
  })
  
  if(!is.null(data)) {
    model |> 
      pharmr::set_dataset(data)
  }
  
  model
}
