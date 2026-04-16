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

  ## When a dictionary was applied in create_model(), use the original data
  ## (with original column names) so the CSV is an exact copy of the input.
  ## NONMEM reads by column position, so the header names don't matter.
  original_data <- attr(model, "original_data")

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
      ## If the source CSV has quoted headers (e.g. `"ID","TIME",...`), NONMEM
      ## will try to parse the header row as data. Detect this and rewrite the
      ## dataset with unquoted headers.
      first_line <- tryCatch(readLines(dataset_path, n = 1), error = function(e) character(0))
      if (length(first_line) && grepl('^["\']', first_line)) {
        if (verbose) cli::cli_alert_info("Stripping quoted column names from dataset header")
        df <- read.csv(dataset_path, check.names = FALSE)
        df <- unquote_column_names(df)
        write.csv(df, file = dataset_path, quote = FALSE, row.names = FALSE)
      }
    } else {
      if(verbose) cli::cli_process_start("Checking, cleaning, and copying dataset")
      data <- unquote_column_names(data)
      if(isTRUE(auto_stack_encounters)) {
        data <- stack_encounters(
          data = data,
          verbose = verbose
        )
      }
      if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset")
      write.csv(data, file = dataset_path, quote = FALSE, row.names = FALSE)
    }
  } else if (!is.null(original_data)) {
    if (verbose) cli::cli_process_start("Copying dataset (original column names)")
    original_data <- unquote_column_names(original_data)
    write.csv(original_data, file = dataset_path, quote = FALSE, row.names = FALSE)
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
  ## Replace dictionary placeholder column names with DROP
  model_code <- gsub("_DDRP_[A-Za-z0-9_]+", "DROP", model_code, perl = TRUE)
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
