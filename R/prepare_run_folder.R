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
  ## Whether to rewrite the model's $DATA record. NONMEM is run from inside
  ## the run folder, so $DATA must point either at the copy in the run folder
  ## (`copy_dataset = TRUE`) or at the dataset's absolute path
  ## (`copy_dataset = FALSE`): a relative path would no longer resolve. The
  ## only case where $DATA is left as-is is when it is already an absolute
  ## path to an existing file.
  update_data_record <- TRUE
  ## Directory a relative $DATA path in the model is relative to: the folder
  ## the model file was read from (see create_model_from_file()), or the
  ## working directory when that is unknown.
  data_dir <- attr(model, "data_dir")
  if(is.null(data_dir)) data_dir <- getwd()
  model_file <- "run.mod"
  output_file <- "run.lst"
  model_path <- file.path(fit_folder, model_file)

  ## When a dictionary was applied in create_model(), use the original data
  ## (with original column names) so the CSV is an exact copy of the input.
  ## NONMEM reads by column position, so the header names don't matter.
  original_data <- attr(model, "original_data")

  ## Every branch below opens exactly one status bar when `verbose`; keep its
  ## id so the close pairs with *that* bar. A bare `cli_process_done()` pops
  ## whatever is on cli's stack, i.e. a caller's progress bar if this function
  ## ever gained a path that does not open one. See issue #137.
  proc <- NULL

  if(!is.null(data)) {
    if(inherits(data, "character")) {
      if(!file.exists(data)) {
        cli::cli_abort(c(
          "`data` file does not exist: {.path {data}}.",
          "i" = "Relative paths are resolved against the working directory ({.path {getwd()}})."
        ))
      }
      if(isTRUE(auto_stack_encounters)) {
        cli::cli_warn("`auto_stack_encounters` can only be used when `data` is specified as data.frame, not when it is a CSV filename.")
      }
      if(!copy_dataset) {
        ## Leave the dataset in its existing location and point $DATA at its
        ## absolute path (resolved against the caller's working directory,
        ## not the run folder NONMEM runs in). The file is not modified (so no
        ## quoted-header rewrite); the user is responsible for the dataset
        ## being NONMEM-ready.
        if(verbose) proc <- cli::cli_process_start("Using dataset in existing location (not copying into run folder)")
        dataset_path <- normalizePath(data, mustWork = TRUE)
      } else {
        if(verbose) proc <- cli::cli_process_start("Copying dataset")
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
      if(verbose) proc <- cli::cli_process_start("Checking, cleaning, and copying dataset")
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
    ## `copy_dataset = FALSE`: leave that file in place and point $DATA at its
    ## absolute path, rather than re-writing the in-memory `original_data` into
    ## the run folder.
    dataset_file <- if(!copy_dataset) get_dataset_path_from_model(model, base_dir = data_dir) else NULL
    if(!is.null(dataset_file)) {
      if (verbose) proc <- cli::cli_process_start("Using dataset from model's $DATA record (not copying into run folder)")
      dataset_path <- normalizePath(dataset_file, mustWork = TRUE)
      update_data_record <- !isTRUE(attr(dataset_file, "absolute"))
    } else {
      if(!copy_dataset) {
        cli::cli_warn(c(
          "!" = "{.code copy_dataset = FALSE} can only be honored when the dataset is a file on disk (supplied via {.arg data} or referenced by the model's $DATA record).",
          "i" = "Only the model's original (in-memory) dataset is available; copying it into the run folder and updating $DATA instead."
        ))
      }
      if (verbose) proc <- cli::cli_process_start("Copying dataset (original column names)")
      original_data <- unquote_column_names(original_data)
      write.csv(original_data, file = dataset_path, quote = FALSE, row.names = FALSE)
    }
  } else {
    ## `data` is NULL: resolve dataset from the model. Try the $DATA record
    ## path first — if it points to a real file we can honor `copy_dataset`.
    ## Only fall back to writing `model$dataset` (in-memory) to the run folder
    ## when no usable on-disk source exists.
    dataset_file <- get_dataset_path_from_model(model, base_dir = data_dir)
    if (!is.null(dataset_file)) {
      if (!copy_dataset) {
        ## Dataset already referenced by $DATA and present on disk: leave the
        ## file in place. A relative $DATA (resolved against the model's
        ## folder) is rewritten to the absolute path, since NONMEM runs in the
        ## run folder; an absolute $DATA is left as-is.
        if (verbose) proc <- cli::cli_process_start("Using dataset from model's $DATA record (not copying into run folder)")
        dataset_path <- normalizePath(dataset_file, mustWork = TRUE)
        update_data_record <- !isTRUE(attr(dataset_file, "absolute"))
      } else {
        if (verbose) proc <- cli::cli_process_start("Copying dataset from model's $DATA record")
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
      if (verbose) proc <- cli::cli_process_start("Copying dataset from model object")
      write.csv(model$dataset, file = dataset_path, quote = FALSE, row.names = FALSE)
    } else {
      data_ref <- get_dataset_ref_from_model(model)
      cli::cli_abort(c(
        "No dataset could be resolved: `model$dataset` is NULL and no existing file was found from the model's $DATA record.",
        "i" = if(!is.null(data_ref)) "$DATA refers to {.path {data_ref}}, which does not exist relative to {.path {data_dir}}."
      ))
    }
  }

  ## Copy modelfile
  model_code <- model$code
  ## Replace dictionary placeholder column names with DROP
  model_code <- gsub("_DDRP_[A-Za-z0-9_]+", "DROP", model_code, perl = TRUE)
  ## Point $DATA at the dataset (run-folder copy or absolute path), unless it
  ## already is an absolute path to the existing file. Only the path token is
  ## replaced; IGNORE=/ACCEPT= and other options are kept.
  if (update_data_record) {
    model_code <- change_nonmem_dataset(
      model_code,
      dataset_path
    )
  }
  writeLines(model_code, model_path)
  if(!is.null(proc)) cli::cli_process_done(id = proc)

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
#' is an existing file on disk (ignoring `IGNORE=`/`ACCEPT=` options). Relative
#' paths are resolved against `base_dir`, the folder the model file was read
#' from, rather than the working directory or the run folder. Returns
#' `NULL` when no element points to an existing file (e.g. $DATA is the
#' `DUMMYPATH` placeholder used while the dataset lives only in memory).
#'
#' @param model pharmpy model object
#' @param base_dir directory relative `$DATA` paths are resolved against.
#' Defaults to the working directory.
#'
#' @returns path to an existing dataset file (character), or `NULL`. The
#' returned path carries an attribute `absolute`, `TRUE` when `$DATA` already
#' held an absolute path.
#'
get_dataset_path_from_model <- function(model, base_dir = getwd()) {
  for (f in get_dataset_elements_from_model(model)) {
    ## `~` is expanded by R but not by NONMEM, so such a path still needs to
    ## be rewritten to the expanded absolute path.
    tilde <- startsWith(f, "~")
    absolute <- fs::is_absolute_path(f) && !tilde
    path <- if (absolute) f else if (tilde) path.expand(f) else file.path(base_dir, f)
    if (file.exists(path) && !dir.exists(path)) {
      return(structure(path, absolute = absolute))
    }
  }
  NULL
}

#' Dataset path as written in a model's $DATA record
#'
#' @param model pharmpy model object
#' @returns the first non-option element of `$DATA` (character), or `NULL`
#' @noRd
get_dataset_ref_from_model <- function(model) {
  elem <- get_dataset_elements_from_model(model)
  if (length(elem)) elem[[1]] else NULL
}

#' Non-option elements of a model's $DATA record, quotes stripped
#'
#' @param model pharmpy model object
#' @returns character vector
#' @noRd
get_dataset_elements_from_model <- function(model) {
  obj <- nm_read_model(code = model$code)
  data_block <- stringr::str_replace_all(obj$DATA, "\\$DATA\\s*", "")
  data_elem <- unlist(stringr::str_split(data_block, "\\s"))
  data_elem <- data_elem[!grepl("(IGNORE=|ACCEPT=)", data_elem)]
  data_elem <- gsub("^[\"']|[\"']$", "", data_elem)
  data_elem[nzchar(data_elem)]
}
