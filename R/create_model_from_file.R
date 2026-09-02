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

  ## Drop bookkeeping columns whose names are not valid NONMEM $INPUT symbols
  ## (e.g. the `.regimen` column create_sim_dataset() attaches). Such a name
  ## cannot be a NONMEM data item, and leaving it in makes Pharmpy's parser
  ## reject the $INPUT token (a leading-dot name is a syntax error, DROP flag or
  ## not). When we drop any, force a freshly written dataset below so $DATA
  ## points at a CSV whose columns match the rewritten $INPUT.
  if(!is.null(data)) {
    valid_input <- grepl("^[A-Za-z][A-Za-z0-9_]*$", names(data))
    if(any(!valid_input)) {
      data <- data[, valid_input, drop = FALSE]
      dataset_file <- NULL
    }
  }

  ## Create Pharmpy object
  tryCatch({
    model_code <- readLines(model_file) |>
      paste(collapse = "\n") |>
      fix_eta_dummy_bug() |>
      strip_input_commas()
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
    }
    model_code <- model$code
    model_path <- tempfile(fileext = ".mod")
    ## Deliberately not using pharmr::set_dataset(datatype = "nonmem") here:
    ## it rewrites $INPUT from the dataframe's columns and thereby discards the
    ## DROP flags declared in the model file's original $INPUT. Pharmpy then
    ## treats non-numeric DROP columns (e.g. date/time strings like
    ## "08/12/2011", "9:00") as numeric and float-converts them, raising a
    ## DatasetError. Instead sync $INPUT to the dataset columns ourselves,
    ## carrying the original tokens (and their DROP flags) over. See #99/#101.
    non_numeric <- names(data)[!vapply(data, is_numeric_column, logical(1))]
    model_code <- sync_input_to_dataset(model_code, names(data), non_numeric) |>
      change_nonmem_dataset(dataset_file) |>
      fix_eta_dummy_bug()
    tryCatch({
      model <- pharmr::read_model_from_string(model_code)
    })
  }
  
  model
}

#' Strip commas from the $INPUT record of NONMEM model code
#'
#' Comma-separated `$INPUT` items are valid NONMEM but not accepted by
#' Pharmpy, which expects space-separated column names. This replaces commas
#' inside the `$INPUT` record with spaces, leaving other records (e.g. the
#' comma-containing `$THETA`/`$OMEGA` bounds) untouched. Matching stops at the
#' next record boundary (`$`).
#'
#' @param text Character string with model code.
#'
#' @returns Character string with model code.
strip_input_commas <- function(text) {
  gsub("(?:\\$INPUT\\b|(?!\\A)\\G)[^,$]*\\K,", " ", text, perl = TRUE)
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

#' Align a model's `$INPUT` record with the columns of a dataset
#'
#' NONMEM reads datasets positionally, so re-pointing `$DATA` at a CSV written
#' from a data.frame requires `$INPUT` to list that data.frame's columns in
#' order. `pharmr::set_dataset(datatype = "nonmem")` does this by regenerating
#' `$INPUT` from the column names alone, which silently drops any `DROP`
#' (or `SKIP`) flags the model declared — after which pharmpy tries to
#' float-convert non-numeric dropped columns and raises a `DatasetError`.
#'
#' This keeps the original token for every column the model already named
#' (so `VISITDATE=DROP` stays `VISITDATE=DROP`), emits a bare `DROP` for
#' pharmpy's placeholder names for anonymous dropped columns (`_DROP1`, ...),
#' and appends any genuinely new column under its own name.
#'
#' @param code character string with NONMEM model code
#' @param columns character vector of dataset column names, in dataset order
#' @returns character string with model code
#' Is a dataset column numeric as far as NONMEM/Pharmpy is concerned?
#'
#' TRUE for a numeric vector, and for a character/factor column whose every
#' non-missing value parses as a number (so `"70"` counts as numeric, matching
#' Pharmpy coercing it to 70). NONMEM missing placeholders (`.`, empty, `NA`)
#' are ignored. FALSE only when a genuine non-numeric string is present (e.g. a
#' treatment-arm label like `"Cohort A"`), which is what makes Pharmpy's float
#' conversion fail.
#'
#' @param x a dataset column
#' @returns TRUE/FALSE
#' @noRd
is_numeric_column <- function(x) {
  if(is.numeric(x)) return(TRUE)
  v <- trimws(as.character(x))
  v <- v[!is.na(v) & v != "" & v != "."]
  if(length(v) == 0) return(TRUE)
  !anyNA(suppressWarnings(as.numeric(v)))
}

#' @param non_numeric character vector of dataset columns whose values are not
#'   numeric. A genuinely new column (one the model never named) that is
#'   non-numeric is emitted as `<col>=DROP` rather than a bare, readable token:
#'   the model cannot reference a column absent from its original `$INPUT`, and
#'   leaving it readable makes pharmpy float-convert its text and raise a
#'   `DatasetError`. New *numeric* columns are still appended bare so they can
#'   be read (e.g. a covariate added alongside the model).
#' @noRd
sync_input_to_dataset <- function(code, columns, non_numeric = character(0)) {
  old_tokens <- unname(get_input_tokens(code))
  if(length(old_tokens) == 0 || length(columns) == 0) return(code)
  old_names <- vapply(old_tokens, input_token_name, character(1), USE.NAMES = FALSE)
  is_anon_drop <- toupper(old_tokens) %in% c("DROP", "SKIP")
  new_tokens <- vapply(seq_along(columns), function(i) {
    column <- columns[i]
    same_pos <- i <= length(old_tokens)
    ## Column still sits where the model declared it: keep the token verbatim,
    ## DROP flag, synonym and all.
    if(same_pos && old_names[i] == column) return(old_tokens[i])
    ## Pharmpy names anonymous `DROP` items `_DROP1`, `_DROP2`, ... in the
    ## dataset it exposes; re-emit those as an anonymous DROP.
    if(grepl("^_DROP[0-9]*$", column)) return("DROP")
    ## Column moved: carry its original token over to the new position.
    match_idx <- which(old_names == column)
    if(length(match_idx) > 0) return(old_tokens[match_idx[1]])
    ## No token names this column, but the model dropped whatever sat at this
    ## position anonymously (e.g. `set_dv()` rewrites the old DV to `DROP`, so
    ## the dataset still carries its original name). Keep it dropped rather
    ## than re-introducing a name that may collide with another token.
    if(same_pos && is_anon_drop[i]) return("DROP")
    ## Genuinely new column the model never named. If it is non-numeric, drop
    ## it: the model cannot reference it, and leaving it readable makes pharmpy
    ## float-convert its text and fail. Numeric new columns stay readable.
    if(column %in% non_numeric) return(paste0(column, "=DROP"))
    column
  }, character(1), USE.NAMES = FALSE)
  if(identical(new_tokens, old_tokens)) return(code)
  rewrite_input_tokens(code, function(tokens) new_tokens)
}

#' Column name referred to by a single `$INPUT` item
#'
#' Handles the plain (`WT`), labelled (`DV=CONC`, where pharmpy names the
#' column after the synonym) and both DROP spellings NONMEM accepts
#' (`VISITDATE=DROP` and `DROP=VISITDATE`).
#'
#' @param token character string, a single `$INPUT` item
#' @returns character string
#' @noRd
input_token_name <- function(token) {
  parts <- strsplit(token, "=", fixed = TRUE)[[1]]
  if(length(parts) < 2) return(parts[1])
  ## `LABEL=SYNONYM`: pharmpy names the dataset column after the synonym
  ## (`DV=CONC` -> `CONC`), except when either side is the DROP/SKIP keyword.
  if(toupper(parts[2]) %in% c("DROP", "SKIP")) return(parts[1])
  if(toupper(parts[1]) %in% c("DROP", "SKIP")) return(parts[2])
  parts[2]
}
