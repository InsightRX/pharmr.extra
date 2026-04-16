#' Create a Pharmpy model object from a model file and dataset (optional)
#' 
#' @param model_file the model file (.mod) to read.
#' @param ext_file optional path to a .ext file containing final parameter 
#'   estimates that will be used to update the initial estimates in the model.
#' @param data the filename of the dataset (or an actual data.frame)
#' @param verbose verbose output
#' 
#' @returns a Pharmpy model object
#'
#' @export
create_model_from_file <- function(
  model_file,
  ext_file = NULL,
  data = NULL,
  verbose = TRUE
) {
  
  ## Checks
  dataset_file <- NULL
  if(! inherits(model_file, "character")) {
    cli::cli_abort("Model file should be a string.")
  }
  if(! file.exists(model_file)) {
    cli::cli_abort("Model file {model_file} does not exist")
  }
  if(inherits(data, "data.frame") || inherits(data, "tibble")) {
    ## Do nothing
  } else if (inherits(data, "character")) {
    dataset_file <- data
    if (!file.exists(dataset_file)) {
      cli::cli_abort("Data file {dataset_file} does not exist")
    }
    data <- read.csv(dataset_file)
  }
  
  ## Create Pharmpy object
  tryCatch({
    model_code <- readLines(model_file) |>
      paste(collapse = "\n") |>
      fix_eta_dummy_bug()
    if(!is.null(data)) {
      ## if `data` supplied, then make sure current path is DUMMYPATH
      ## otherwise, if it points to a file that does not exists,
      ## Pharmpy will fail
      model_code <- change_nonmem_dataset(model_code, "DUMMYPATH")
    }
    model <- pharmr::read_model_from_string(model_code)
  })

  ## If .ext file provided, update initial estimates
  if(!is.null(ext_file)) {
    if(file.exists(ext_file)) {
      cli::cli_alert_info("Updating initial estimates for model using {ext_file}.")
      model_id <- basename(tools::file_path_sans_ext(model_file))
      tmp_dir <- tempfile(pattern = paste0(model_id, "_"), tmpdir = tempdir())
      dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
      ext_dest <- file.path(tmp_dir, paste0(model_id, ".ext"))
      mod_dest <- file.path(tmp_dir, paste0(model_id, ".mod"))
      copied_ext <- file.copy(ext_file, ext_dest)
      if (!isTRUE(copied_ext)) {
        cli::cli_abort("Failed to copy ext file from {ext_file} to {ext_dest}.")
      }
      copied_mod <- file.copy(model_file, mod_dest)
      if (!isTRUE(copied_mod)) {
        cli::cli_abort("Failed to copy model file from {model_file} to {mod_dest}.")
      }
      fit <- pharmr::read_modelfit_results(
        path = mod_dest
      )
      model <- model |>
        pharmr.extra::update_parameters(fit)
    } else {
      cli::cli_abort("Supplied `ext_file` ({ext_file}) does not exist")
    } 
  }
  
  if(!is.null(data)) {
    if(is.null(dataset_file)) {
      dataset_file <- tempfile(pattern = "data", fileext = ".csv")
      write.csv(data, dataset_file, quote = F, row.names = F)
      model <- model |>
        pharmr::set_dataset(
          path_or_df = dataset_file, datatype = "nonmem") |>
        pharmr::load_dataset()
    }
    model_code <- model$code
    model_path <- tempfile(fileext = ".mod")
    model_code <- change_nonmem_dataset(
      model_code,
      dataset_file
    ) |>
      fix_eta_dummy_bug()
    tryCatch({
      model <- pharmr::read_model_from_string(model_code)
    })
  }
  
  model
}

#' Guard against a bug in Pharmpy where eta_dummy is not correctly imported
#' 
#' @param model_code Character string with model code.
#' 
#' @returns Character string with model code.
fix_eta_dummy_bug <- function(model_code) {
  # We intentionally scan the entire model code (including comments) because
  # eta_dummy is only expected to appear as this placeholder name.
  # Use a whole-word match to avoid changing substrings like "meta_dummy".
  pattern <- stringr::regex("\\beta_dummy\\b", ignore_case = FALSE)
  if (stringr::str_detect(model_code, pattern)) {
    model_code <- stringr::str_replace_all(model_code, pattern, "ETA_DUMMY")
  }
  model_code
}
