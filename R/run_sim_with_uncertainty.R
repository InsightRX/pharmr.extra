#' Simulate from a NONMEM model with parameter uncertainty
#'
#' Uses PsN's `vpc --uncertainty_sample` to draw parameter vectors from the
#' multivariate normal distribution defined by the NONMEM covariance matrix,
#' run one simulation per draw, and return all results combined in a single
#' data.frame.
#'
#' @inheritParams run_sim
#' @param fit a Pharmpy modelfit object returned by [run_nlme()]. Used to
#' locate the run folder. Either `fit` or `fit_folder` must be supplied.
#' @param fit_folder path to an existing NONMEM run folder that contains
#' `run.mod`, `run.cov`, and `run.ext`. An alternative to supplying `fit`.
#' @param n_samples number of parameter vectors to sample from the covariance
#' matrix. Each sample becomes one simulated dataset. Default `200`.
#' @param data optional `data.frame` to use as the simulation dataset instead
#' of the dataset stored in `fit_folder`. Column order must match the model's
#' `$INPUT` record.
#' @param variables character vector of variable names to include in the output
#' table. If `NULL`, defaults to `c("ID", "TIME", "DV", "EVID", "PRED")` plus
#' all variables declared in the model code.
#' @param output_file name of the NONMEM `$TABLE FILE=` (default `"simtab"`).
#' @param seed random seed passed to PsN (`--seed`).
#' @param id run identifier used for the working directory name.
#' @param path directory in which to create the vpc working folder. Defaults
#' to the current working directory.
#' @param force if the working directory already exists, overwrite it?
#' @param psn_extra_args optional character vector of additional arguments to
#' pass to PsN vpc, e.g. `c("--stratify_on=GRP")`.
#' @param console stream PsN stdout/stderr to the R console? Default `FALSE`
#' (written to files inside the run folder).
#' @param verbose print progress messages?
#'
#' @returns A `data.frame` with all simulation results. A `sample_id` column
#' (integer) identifies which uncertainty sample each row belongs to.
#'
#' @export
run_sim_with_uncertainty <- function(
  fit = NULL,
  fit_folder = NULL,
  n_samples = 200,
  data = NULL,
  variables = NULL,
  output_file = "simtab",
  seed = 12345,
  id = irxutils::get_random_id("vpc_unc_"),
  path = getwd(),
  force = FALSE,
  psn_extra_args = NULL,
  console = FALSE,
  verbose = TRUE
) {

  ## --- 1. Resolve fit_folder -----------------------------------------------
  if (!is.null(fit)) {
    resolved <- extract_fit_folder(fit)
    if (is.null(resolved)) {
      cli::cli_abort(c(
        "Could not determine run folder from `fit` object.",
        "i" = "Supply `fit_folder` directly, e.g. `fit_folder = 'run1/'`."
      ))
    }
    fit_folder <- resolved
  } else if (!is.null(fit_folder)) {
    fit_folder <- normalizePath(fit_folder, mustWork = TRUE)
  } else {
    cli::cli_abort("Supply either a `fit` object or a `fit_folder` path.")
  }

  if (verbose) cli::cli_alert_info("Using fit folder: {fit_folder}")

  ## --- 2. Verify required files --------------------------------------------
  required <- c("run.mod", "run.cov", "run.ext")
  missing_files <- required[!file.exists(file.path(fit_folder, required))]
  if (length(missing_files) > 0) {
    missing_str <- paste(missing_files, collapse = ", ")
    abort_msg <- c(
      "Required files not found in fit folder ({fit_folder}): {missing_str}"
    )
    if ("run.cov" %in% missing_files) {
      abort_msg <- c(
        abort_msg,
        "i" = "A covariance matrix (run.cov) is required for uncertainty sampling.",
        "i" = "Re-run `run_nlme()` with a `$COVARIANCE` step in the model."
      )
    }
    cli::cli_abort(abort_msg)
  }

  ## --- 3. Load model and set up $TABLE -------------------------------------
  if (verbose) cli::cli_process_start("Loading model and configuring output table")

  model <- create_model_from_file(
    model_file = file.path(fit_folder, "run.mod")
  )

  if (is.null(variables)) {
    default_variables <- c("ID", "TIME", "DV", "EVID", "PRED")
    variables <- c(default_variables, get_declared_variables(model))
  }

  ## Filter to variables that are valid in this model
  checked_variables <- c()
  for (variab in variables) {
    check_var <- check_nm_table_variables(model, variab, throw_error = FALSE)
    if (is.null(check_var)) {
      checked_variables <- c(checked_variables, variab)
    }
  }
  parameter_names <- get_defined_pk_parameters(model)
  checked_variables <- unique(c(checked_variables, parameter_names))

  ## Replace $TABLE records — keep $ESTIMATION intact (PsN vpc requires it)
  model <- model |>
    remove_tables_from_model() |>
    add_table_to_model(checked_variables, file = output_file)

  if (verbose) cli::cli_process_done()

  ## --- 4. Create vpc working directory -------------------------------------
  vpc_folder <- file.path(path, id)
  if (dir.exists(vpc_folder)) {
    if (isTRUE(force)) {
      unlink(vpc_folder, recursive = TRUE)
    } else {
      cli::cli_abort(
        c(
          "VPC working folder already exists: {vpc_folder}",
          "i" = "Use `force = TRUE` to overwrite it."
        )
      )
    }
  }
  dir.create(vpc_folder, showWarnings = FALSE, recursive = TRUE)

  ## --- 5. Write model + dataset + covariance files -------------------------
  if (verbose) cli::cli_process_start("Preparing vpc working folder")

  dataset_in_vpc <- file.path(vpc_folder, "data.csv")
  model_in_vpc   <- file.path(vpc_folder, "run.mod")

  ## Dataset
  if (!is.null(data)) {
    if (!inherits(data, "data.frame")) {
      cli::cli_abort("`data` must be a data.frame.")
    }
    ## Ensure column order matches original dataset if possible
    original_data <- model$dataset
    if (!is.null(original_data)) {
      orig_cols <- names(original_data)
      data_cols  <- names(data)
      shared <- intersect(orig_cols, data_cols)
      data <- data[, shared, drop = FALSE]
    }
    write.csv(data, dataset_in_vpc, quote = FALSE, row.names = FALSE)
  } else {
    orig_dataset <- file.path(fit_folder, "data.csv")
    if (!file.exists(orig_dataset)) {
      cli::cli_abort("No dataset found at {orig_dataset}. Supply `data` directly.")
    }
    file.copy(orig_dataset, dataset_in_vpc)
  }

  ## Model code — update $DATA path, then write
  model_code <- model$code
  model_code <- change_nonmem_dataset(model_code, dataset_in_vpc)
  writeLines(model_code, model_in_vpc)

  ## Covariance and estimates files (must share the "run" base name with run.mod)
  file.copy(file.path(fit_folder, "run.cov"), file.path(vpc_folder, "run.cov"))
  file.copy(file.path(fit_folder, "run.ext"), file.path(vpc_folder, "run.ext"))

  if (verbose) cli::cli_process_done()

  ## --- 6. Run PsN vpc -------------------------------------------------------
  vpc_dir_name <- paste0("vpc_dir_", id)

  psn_options <- c(
    paste0("--uncertainty_sample=", n_samples),
    "--samples=1",
    paste0("--seed=", seed),
    paste0("--dir=", vpc_dir_name),
    psn_extra_args
  )

  if (verbose) {
    cli::cli_alert_info(
      "Running PsN vpc with {n_samples} uncertainty samples (this may take a while)"
    )
  }

  call_psn(
    model_file  = "run.mod",
    output_file = "run.lst",
    path        = vpc_folder,
    options     = psn_options,
    tool        = "vpc",
    console     = console,
    verbose     = verbose
  )

  ## --- 7. Locate vpc output directory --------------------------------------
  ## PsN may auto-increment the dir name if it already exists, so glob for it
  vpc_output_candidates <- list.dirs(vpc_folder, full.names = TRUE, recursive = FALSE)
  vpc_output_candidates <- vpc_output_candidates[
    grepl(paste0("^", vpc_dir_name), basename(vpc_output_candidates))
  ]
  if (length(vpc_output_candidates) == 0) {
    cli::cli_abort(
      c(
        "PsN vpc output directory not found under {vpc_folder}.",
        "i" = "Check PsN output for errors (set `console = TRUE` to see it)."
      )
    )
  }
  vpc_output_dir <- vpc_output_candidates[1]

  ## --- 8. Parse and return simulation results -------------------------------
  if (verbose) cli::cli_process_start("Reading simulation tables")
  result <- read_vpc_sim_tables(vpc_output_dir, table_name = output_file)
  if (verbose) cli::cli_process_done()

  if (verbose) cli::cli_alert_success("Done. Returned {nrow(result)} rows from {n_samples} uncertainty samples.")

  result
}


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

#' Extract the run folder path from a pharmr.extra fit object
#'
#' @param fit modelfit object returned by [run_nlme()]
#' @returns character path, or NULL if it cannot be determined
#'
extract_fit_folder <- function(fit) {
  model <- attr(fit, "model")
  if (is.null(model)) return(NULL)

  path_obj <- model$path
  if (is.null(path_obj)) return(NULL)

  ## reticulate exposes Python's NoneType as a special class
  if (inherits(path_obj, "python.builtin.NoneType")) return(NULL)

  model_file_str <- tryCatch(
    as.character(path_obj),
    error = function(e) NULL
  )
  if (is.null(model_file_str) || !nzchar(model_file_str)) return(NULL)

  normalizePath(dirname(model_file_str), mustWork = FALSE)
}


#' Read simulation tables from PsN vpc uncertainty-sample output
#'
#' @param vpc_dir path to the PsN vpc output directory (contains `m1/`)
#' @param table_name name of the simulation table file (default `"simtab"`)
#'
#' @returns data.frame with all rows combined and a `sample_id` integer column
#'
read_vpc_sim_tables <- function(vpc_dir, table_name = "simtab") {

  m1_dir <- file.path(vpc_dir, "m1")
  if (!dir.exists(m1_dir)) {
    cli::cli_abort("Expected `m1/` subdirectory not found in {vpc_dir}.")
  }

  ## Find all NM_runN directories
  all_dirs   <- list.dirs(m1_dir, full.names = TRUE, recursive = FALSE)
  nm_run_dirs <- all_dirs[grepl("NM_run[0-9]+$", basename(all_dirs))]

  if (length(nm_run_dirs) == 0) {
    cli::cli_abort("No NM_run* directories found in {m1_dir}.")
  }

  ## Sort numerically (not lexicographically)
  run_numbers <- as.integer(stringr::str_extract(basename(nm_run_dirs), "[0-9]+$"))
  ord <- order(run_numbers)
  nm_run_dirs <- nm_run_dirs[ord]
  run_numbers <- run_numbers[ord]

  results  <- vector("list", length(nm_run_dirs))
  n_failed <- 0L

  for (i in seq_along(nm_run_dirs)) {
    table_path <- file.path(nm_run_dirs[i], table_name)
    if (!file.exists(table_path)) {
      cli::cli_warn("Simulation table not found for NM_run{run_numbers[i]}: {table_path}. Skipping.")
      n_failed <- n_failed + 1L
      next
    }
    tbl <- tryCatch(
      suppressWarnings(suppressMessages(read_table_nm(file = table_path))),
      error = function(e) {
        cli::cli_warn(
          "Could not read table for NM_run{run_numbers[i]}: {conditionMessage(e)}"
        )
        n_failed <<- n_failed + 1L
        NULL
      }
    )
    if (!is.null(tbl)) {
      tbl$sample_id <- run_numbers[i]
      results[[i]] <- tbl
    }
  }

  n_ok <- length(nm_run_dirs) - n_failed
  if (n_ok == 0) {
    cli::cli_abort("No simulation tables could be read from {vpc_dir}. Check PsN output for errors.")
  }
  if (n_failed > 0) {
    cli::cli_warn(
      "{n_failed} of {length(nm_run_dirs)} uncertainty sample(s) had no readable output."
    )
  }

  dplyr::bind_rows(results)
}
