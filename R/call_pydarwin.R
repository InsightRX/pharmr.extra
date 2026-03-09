#' Run a pyDarwin model search
#'
#' Calls pyDarwin's `run_search()` via reticulate. Requires pyDarwin to be
#' installed in the Python environment used by reticulate (see
#' [install_pydarwin()]).
#'
#' @param template_file Path to the pyDarwin template file (e.g. `template.txt`).
#'   The NONMEM control stream with token placeholders.
#' @param tokens_file Path to the tokens JSON file (e.g. `tokens.json`).
#' @param options_file Path to an options JSON file (e.g. `options.json`).
#'   If `NULL`, options must be supplied via the `options` argument.
#'   If both `options_file` and `options` are provided, values in `options`
#'   are merged on top of the file (R list takes precedence).
#' @param options Named R list of pyDarwin options. Merged with `options_file`
#'   if provided, or used as the sole source of options if `options_file` is
#'   `NULL`. Use [pydarwin_options()] to build this list with documentation.
#' @param nmfe Path to the NONMEM `nmfe` executable. If `NULL` (default),
#'   auto-detected via [get_nmfe_location()] using the Pharmpy configuration.
#'   This value is written into `options$nmfe_path` if not already set there.
#' @param folder Working directory for the search. Defaults to [getwd()].
#'   pyDarwin will write its output relative to the directories configured in
#'   `options` (e.g. `working_dir`, `output_dir`). If those are not set in
#'   options, pyDarwin uses the current working directory.
#' @param verbose Emit progress messages?
#'
#' @return A named list with:
#'   - `output_dir`: path to the pyDarwin output directory
#'   - `results`: data frame parsed from `results.csv` (or `NULL` if not found)
#'
#' @export
call_pydarwin <- function(
  template_file,
  tokens_file,
  options_file = NULL,
  options = list(),
  nmfe = NULL,
  folder = NULL,
  verbose = TRUE
) {

  ## ---- validate inputs -------------------------------------------------------
  if (!file.exists(template_file)) {
    cli::cli_abort("Template file not found: {template_file}")
  }
  if (!file.exists(tokens_file)) {
    cli::cli_abort("Tokens file not found: {tokens_file}")
  }
  if (!is.null(options_file) && !file.exists(options_file)) {
    cli::cli_abort("Options file not found: {options_file}")
  }
  if (is.null(options_file) && length(options) == 0) {
    cli::cli_abort(
      "Please supply either `options_file` or a non-empty `options` list."
    )
  }

  ## ---- resolve nmfe path -----------------------------------------------------
  nmfe <- get_nmfe_location(nmfe = nmfe, verbose = verbose)

  ## ---- build effective options list ------------------------------------------
  effective_options <- if (!is.null(options_file)) {
    base <- jsonlite::read_json(options_file)
    ## merge: values from R `options` list override the file
    utils::modifyList(base, options)
  } else {
    options
  }

  ## inject nmfe_path if not already present in options
  if (is.null(effective_options$nmfe_path)) {
    effective_options$nmfe_path <- nmfe
  }

  ## determine output directory (for reading results afterwards)
  output_dir <- effective_options$output_dir %||% folder %||% getwd()

  ## ---- resolve folder --------------------------------------------------------
  if (is.null(folder)) folder <- getwd()

  ## ---- write effective options to a temp JSON file ---------------------------
  tmp_options_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_options_file), add = TRUE)
  jsonlite::write_json(effective_options, tmp_options_file, auto_unbox = TRUE, pretty = TRUE)

  ## ---- check darwin is available ---------------------------------------------
  if (!reticulate::py_module_available("darwin")) {
    cli::cli_abort(c(
      "The {.pkg darwin} Python module is not available.",
      "i" = "Install it with {.run pharmr.extra::install_pydarwin()}"
    ))
  }

  darwin <- reticulate::import("darwin")

  ## ---- run search ------------------------------------------------------------
  template_file <- normalizePath(template_file, mustWork = TRUE)
  tokens_file   <- normalizePath(tokens_file,   mustWork = TRUE)

  if (verbose) {
    cli::cli_alert_info("Starting pyDarwin search in {folder}")
  }

  tryCatch({
    withr::with_dir(folder, {
      darwin$run_search(template_file, tokens_file, tmp_options_file)
    })
  }, error = function(e) {
    cli::cli_abort("pyDarwin error: {e}")
  })

  if (verbose) cli::cli_alert_success("pyDarwin search finished.")

  ## ---- collect results -------------------------------------------------------
  results <- read_pydarwin_results(output_dir, verbose = verbose)

  list(
    output_dir = output_dir,
    results    = results
  )
}


#' Build a pyDarwin options list
#'
#' Convenience helper that documents the most commonly used pyDarwin options
#' and provides sensible defaults. Pass the result as the `options` argument
#' to [call_pydarwin()].
#'
#' @param algorithm Search algorithm. One of `"EX"` (exhaustive), `"GA"`
#'   (genetic algorithm), `"GP"` (Gaussian process), `"RF"` (random forest),
#'   `"GBRT"` (gradient-boosted random trees), `"PSO"` (particle swarm),
#'   `"MOGA"`, or `"MOGA3"`.
#' @param nmfe_path Path to the NONMEM `nmfe` executable. If `NULL`, auto-
#'   detected by [call_pydarwin()] via [get_nmfe_location()].
#' @param num_parallel Number of parallel NONMEM runs.
#' @param num_generations Number of search generations (not used for `"EX"`).
#' @param population_size Population size per generation (not used for `"EX"`).
#' @param working_dir Main working directory for pyDarwin. Defaults to the
#'   current working directory at run time.
#' @param output_dir Directory for output files (`results.csv` etc.). Defaults
#'   to `working_dir` if `NULL`.
#' @param project_name Optional project name string.
#' @param random_seed Integer random seed for reproducibility.
#' @param model_run_timeout Timeout per NONMEM run in seconds.
#' @param ... Additional options passed through verbatim. Refer to the
#'   [pyDarwin options documentation](https://certara.github.io/pyDarwin/html/Options.html)
#'   for a full list.
#'
#' @return A named list suitable for passing to the `options` argument of
#'   [call_pydarwin()].
#'
#' @export
pydarwin_options <- function(
  algorithm         = "GA",
  nmfe_path         = NULL,
  num_parallel      = 4L,
  num_generations   = 6L,
  population_size   = 50L,
  working_dir       = NULL,
  output_dir        = NULL,
  project_name      = NULL,
  random_seed       = 11L,
  model_run_timeout = 1200L,
  ...
) {
  opts <- list(
    algorithm         = algorithm,
    num_parallel      = num_parallel,
    num_generations   = num_generations,
    population_size   = population_size,
    random_seed       = random_seed,
    model_run_timeout = model_run_timeout
  )
  if (!is.null(nmfe_path))    opts$nmfe_path    <- nmfe_path
  if (!is.null(working_dir))  opts$working_dir  <- working_dir
  if (!is.null(output_dir))   opts$output_dir   <- output_dir
  if (!is.null(project_name)) opts$project_name <- project_name

  extras <- list(...)
  if (length(extras) > 0) opts <- utils::modifyList(opts, extras)

  opts
}


`%||%` <- rlang::`%||%`
