#' Create a folder for running model, with the model and dataset
#'
prepare_run_folder <- function(
  id,
  model,
  path,
  force = FALSE,
  data = NULL,
  auto_stack_encounters = FALSE,
  verbose = TRUE
) {

  ## Create the folder
  fit_folder <- create_run_folder(
    id = id,
    path,
    force = force,
    verbose
  )

  ## Set up other files
  dataset_path <- file.path(fit_folder, "data.csv")
  model_file <- "run.mod"
  output_file <- "run.lst"
  model_path <- file.path(fit_folder, model_file)

  if(!is.null(data)) {
    if(inherits(data, "character")) {
      if(verbose) cli::cli_process_start("Copying dataset")
      if(!file.exists(data)) {
        cli::cli_abort("`data` file does not exist.")
      }
      if(isTRUE(auto_stack_encounters)) {
        cli::cli_warn("`auto_stack_encounters` can only be used when `data` is specified as data.frame, not when it is a CSV filename.")
      }
      file.copy(from = data, to = dataset_path)
    } else {
      if(verbose) cli::cli_process_start("Checking, cleaning, and copying dataset")
      if(isTRUE(auto_stack_encounters)) {
        data <- stack_encounters(
          data = data,
          verbose = verbose
        )
      }
      if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset")
      model <- model |>
        pharmr::unload_dataset() |>
        pharmr::set_dataset(
          path_or_df = data,
          datatype = "nonmem"
        )
      model <- clean_modelfit_data(model)
      data <- model$dataset
      write.csv(data, file = dataset_path, quote=F, row.names=F)
    }
  } else {
    # When `data` is NULL, prefer using an in-memory dataset if available
    if (!is.null(model$dataset)) {
      if (verbose) cli::cli_process_start("Copying dataset from model object")
      write.csv(model$dataset, file = dataset_path, quote = FALSE, row.names = FALSE)
    } else {
      obj <- nm_read_model(code = model$code)
      data_block <- stringr::str_replace_all(obj$DATA, "\\$DATA\\s*", "")
      data_elem <- unlist(stringr::str_split(data_block, "\\s"))
      data_elem <- data_elem[!grepl("(IGNORE=|ACCEPT=)", data_elem)]
      dataset_file <- NULL
      for (f in data_elem) {
        if (file.exists(f)) {
          dataset_file <- f
          break()
        }
      }
      if (!is.null(dataset_file)) {
        file.copy(from = dataset_file, to = dataset_path)
      } else {
        cli::cli_abort("No dataset could be resolved: `model$dataset` is NULL and no existing file was found from the model's $DATA record.")
      }
    }
  }

  ## Copy modelfile
  model_code <- model$code
  model_code <- change_nonmem_dataset(
    model_code,
    dataset_path
  )
  writeLines(model_code, model_path)
  if(verbose) cli::cli_process_done()

  list(
    model = model,
    model_file = model_file,
    output_file = output_file,
    fit_folder = fit_folder,
    dataset_path = dataset_path
  )
}
