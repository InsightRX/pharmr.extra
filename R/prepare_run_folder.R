#' Create a folder for running model, with the model and dataset
#'
prepare_run_folder <- function(
  id,
  model,
  path,
  force = FALSE,
  data = NULL,
  auto_stack_encounters = FALSE,
  copy_dataset = TRUE,
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
  ## Whether to rewrite the model's $DATA record. Only do so when the dataset
  ## is actually placed into the run folder (copied/written). When the dataset
  ## is left in its existing location (`copy_dataset = FALSE`), $DATA is left
  ## untouched so the model's original data reference is preserved verbatim.
  update_data_record <- TRUE
  model_file <- "run.mod"
  output_file <- "run.lst"
  model_path <- file.path(fit_folder, model_file)

  ## When a dictionary was applied in create_model(), use the original data
  ## (with original column names) so the CSV is an exact copy of the input.
  ## NONMEM reads by column position, so the header names don't matter.
  original_data <- attr(model, "original_data")

  if(!is.null(data)) {
    if(inherits(data, "character")) {
      if(!file.exists(data)) {
        cli::cli_abort("`data` file does not exist.")
      }
      if(isTRUE(auto_stack_encounters)) {
        cli::cli_warn("`auto_stack_encounters` can only be used when `data` is specified as data.frame, not when it is a CSV filename.")
      }
      if(!copy_dataset) {
        ## Leave the dataset in its existing location and leave the model's
        ## $DATA record untouched. The file is not modified (so no quoted-header
        ## rewrite); the user is responsible for the dataset being NONMEM-ready
        ## and for $DATA already pointing at it correctly.
        if(verbose) cli::cli_process_start("Using dataset in existing location (not copying into run folder, $DATA left unchanged)")
        dataset_path <- normalizePath(data, mustWork = TRUE)
        update_data_record <- FALSE
      } else {
        if(verbose) cli::cli_process_start("Copying dataset")
        if(!isTRUE(file.copy(from = data, to = dataset_path))) {
          cli::cli_abort("Failed to copy dataset from {.path {data}} to {.path {dataset_path}}.")
        }
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
      }
    } else {
      if(!copy_dataset) {
        cli::cli_warn(c(
          "!" = "{.code copy_dataset = FALSE} can only be honored when the dataset is a file on disk (supplied via {.arg data} or referenced by the model's $DATA record).",
          "i" = "An in-memory data frame was supplied via {.arg data}; copying it into the run folder and updating $DATA instead."
        ))
      }
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
    ## When `copy_dataset = FALSE` and the model's $DATA record already points
    ## to an existing file (e.g. create_model() wrote the in-memory dataset to
    ## a temp CSV and pointed $DATA at it, so it is no longer DUMMYPATH), honor
    ## `copy_dataset = FALSE`: leave that file in place and leave $DATA
    ## untouched, rather than re-writing the in-memory `original_data` into the
    ## run folder.
    dataset_file <- if(!copy_dataset) get_dataset_path_from_model(model) else NULL
    if(!is.null(dataset_file)) {
      if (verbose) cli::cli_process_start("Using dataset from model's $DATA record (not copying into run folder, $DATA left unchanged)")
      dataset_path <- normalizePath(dataset_file, mustWork = TRUE)
      update_data_record <- FALSE
    } else {
      if(!copy_dataset) {
        cli::cli_warn(c(
          "!" = "{.code copy_dataset = FALSE} can only be honored when the dataset is a file on disk (supplied via {.arg data} or referenced by the model's $DATA record).",
          "i" = "Only the model's original (in-memory) dataset is available; copying it into the run folder and updating $DATA instead."
        ))
      }
      if (verbose) cli::cli_process_start("Copying dataset (original column names)")
      original_data <- unquote_column_names(original_data)
      write.csv(original_data, file = dataset_path, quote = FALSE, row.names = FALSE)
    }
  } else {
    ## `data` is NULL: resolve dataset from the model. Try the $DATA record
    ## path first — if it points to a real file we can honor `copy_dataset`.
    ## Only fall back to writing `model$dataset` (in-memory) to the run folder
    ## when no usable on-disk source exists.
    dataset_file <- get_dataset_path_from_model(model)
    if (!is.null(dataset_file)) {
      if (!copy_dataset) {
        ## Dataset already referenced by $DATA and present on disk: leave both
        ## the file and the $DATA record untouched.
        if (verbose) cli::cli_process_start("Using dataset from model's $DATA record (not copying into run folder, $DATA left unchanged)")
        dataset_path <- normalizePath(dataset_file, mustWork = TRUE)
        update_data_record <- FALSE
      } else {
        if (verbose) cli::cli_process_start("Copying dataset from model's $DATA record")
        if (!isTRUE(file.copy(from = dataset_file, to = dataset_path))) {
          cli::cli_abort("Failed to copy dataset from {.path {dataset_file}} to {.path {dataset_path}}.")
        }
      }
    } else if (!is.null(model$dataset)) {
      if(!copy_dataset) {
        cli::cli_warn(c(
          "!" = "{.code copy_dataset = FALSE} can only be honored when the dataset is a file on disk (supplied via {.arg data} or referenced by the model's $DATA record).",
          "i" = "The model's $DATA record does not point to an existing file; falling back to the in-memory {.code model$dataset}, copying it into the run folder and updating $DATA."
        ))
      }
      if (verbose) cli::cli_process_start("Copying dataset from model object")
      write.csv(model$dataset, file = dataset_path, quote = FALSE, row.names = FALSE)
    } else {
      cli::cli_abort("No dataset could be resolved: `model$dataset` is NULL and no existing file was found from the model's $DATA record.")
    }
  }

  ## Copy modelfile
  model_code <- model$code
  ## Replace dictionary placeholder column names with DROP
  model_code <- gsub("_DDRP_[A-Za-z0-9_]+", "DROP", model_code, perl = TRUE)
  ## Only rewrite $DATA when the dataset was placed into the run folder. When
  ## the dataset is left in place (`copy_dataset = FALSE`), preserve the
  ## model's original $DATA record verbatim.
  if (update_data_record) {
    model_code <- change_nonmem_dataset(
      model_code,
      dataset_path
    )
  }
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

#' Resolve an on-disk dataset path from a model's $DATA record
#'
#' Parses the $DATA record of a NONMEM model and returns the first element that
#' is an existing file on disk (ignoring `IGNORE=`/`ACCEPT=` options). Returns
#' `NULL` when no element points to an existing file (e.g. $DATA is the
#' `DUMMYPATH` placeholder used while the dataset lives only in memory).
#'
#' @param model pharmpy model object
#'
#' @returns path to an existing dataset file (character), or `NULL`
#'
get_dataset_path_from_model <- function(model) {
  obj <- nm_read_model(code = model$code)
  data_block <- stringr::str_replace_all(obj$DATA, "\\$DATA\\s*", "")
  data_elem <- unlist(stringr::str_split(data_block, "\\s"))
  data_elem <- data_elem[!grepl("(IGNORE=|ACCEPT=)", data_elem)]
  for (f in data_elem) {
    if (nzchar(f) && file.exists(f)) {
      return(f)
    }
  }
  NULL
}
