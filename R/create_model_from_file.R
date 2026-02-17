#' Create a Pharmpy model object from a model file and dataset (optional)
#' 
#' @param model_file the model file (.mod) to read. If a file with the same
#' base name but with extension .ext is present, will also read the final 
#' estimates for the run, and update in new model.
#' @param ext_file optional path to a .ext file containing final parameter 
#'   estimates that will be used to update the initial estimates in the model.
#' @param data the filename of the dataset (or an actual data.frame)
#' 
#' @returns a Pharmpy model object
#'
#' @export
create_model_from_file <- function(
  model_file,
  ext_file = NULL,
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
  
  ## If .ext file provided, update initial estimates
  if(!is.null(ext_file)) {
    if(file.exists(ext_file)) {
      cli::cli_alert_info("Updating initial estimates for model using {ext_file}.")
      model_id <- basename(tools::file_path_sans_ext(model_file))
      tmp_dir <- tempdir()
      file.copy(ext_file, file.path(tmp_dir, paste0(model_id , ".ext")))
      file.copy(model_file, file.path(tmp_dir, paste0(model_id, ".mod")))
      fit <- pharmr::read_modelfit_results(
        path = file.path(tmp_dir, paste0(model_id, ".mod"))
      )
      model <- model |>
        pharmr.extra::update_parameters(fit)
    } else {
      cli::cli_abort("Supplied `ext_file` ({ext_file}) does not exist")
    } 
  }
  
  if(!is.null(data)) {
    model <- model |> 
      pharmr::set_dataset(data)
  }
  
  model
}
