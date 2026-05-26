#' Generic function for running a pharmpy tool, like bootstrap,
#' or modelsearch. A separate function is available for `fit()`
#'
#' @param id model id. Optional. If not specified, will generate random modelfit
#' id. The `id` will be used to create the run folder.
#' @param model Pharmpy model object, preferably created using `create_model()`.
#' @param results TODO
#' @param tool TODO
#' @param folder TODO
#' @param clean if one or more run folders exists for the tool,
#' do we want to remove them first?
#' @param verbose verbose output?
#' @param force TODO
#' @param options list of arguments pass on to `tool` as argument. Documentation
#' for available arguments for each Pharmpy tool can be found here:
#' https://pharmpy.github.io/latest/mfl.html.
#' @param remove_tables if `TRUE` (default), removes all `$TABLE` records from the model
#' before passing it to the Pharmpy tool.
#' @param uppercase_mfl if `TRUE` (default), uppercases the model's `$INPUT` /
#' datainfo / dataset column names and the `options$search_space` string before
#' calling the Pharmpy tool. Works around Pharmpy's MFL parser, which
#' unconditionally uppercases every identifier in a search_space and then fails
#' the case-sensitive lookup against datainfo (see
#' <https://github.com/pharmpy/pharmpy/issues/4576>). Set to `FALSE` to disable.
#'
#' @return fit object
#'
#' @examples
#' \dontrun{
#' # Run 200 bootstrap samples on a fitted model
#' bs <- call_pharmpy_tool(
#'   id      = "run1",
#'   model   = model,
#'   results = results,
#'   tool    = "bootstrap",
#'   options = list(samples = 200)
#' )
#'
#' # Inspect parameter estimates (one row per sample)
#' head(as.data.frame(bs$parameter_estimates))
#'
#' # Plot distributions and overlay original estimates
#' orig <- setNames(results$parameter_estimates$estimates,
#'                  results$parameter_estimates$parameter)
#' plot_bootstrap(bs, original_estimates = orig)
#' }
#'
#' @export
call_pharmpy_tool <- function(
  id,
  model = NULL,
  results = NULL,
  tool = NULL,
  folder = NULL,
  clean = TRUE,
  verbose = TRUE,
  force = FALSE,
  options = list(),
  remove_tables = TRUE,
  uppercase_mfl = TRUE
) {

  if(is.null(tool)) {
    cli::cli_abort("Please provide Pharmpy `tool` to run.")
  }
  if(is.null(model) && is.null(results)) {
    cli::cli_abort("Please provide `model` and/or `results` to start Pharmpy tool.")
  }
  if(is.null(model)) {
    if(!is.null(attr(results, "model"))) {
      if(verbose)
        cli::cli_alert_info("No `model` provided, taking from `results` object")
      model <- attr(results, "model")
    } else {
      cli::cli_abort("Please provide `model` to start Pharmpy tool.")
    }
  }

  ## Pharmpy can drive nlmixr2 for modelsearch / covsearch / iivsearch /
  ## ruvsearch / amd / bootstrap (each candidate model is fit via
  ## pharmpy.tools.fit, which honours `esttool`). Upstream support is
  ## fairly young, so we surface a one-time info note pointing at the
  ## runtime requirements; the actual fitting is dispatched to pharmpy.
  ##
  ## Requirements when running these tools against an nlmixr-format model:
  ##   - Python package `pyreadr` installed (pharmpy reads nlmixr fit
  ##     results from .RData files).
  ##   - The system `Rscript` that pharmpy invokes must have a working
  ##     nlmixr2 install compatible with its data.table version.
  engine <- get_tool_from_model(model)
  search_tools <- c(
    "modelsearch", "covsearch", "iivsearch", "ruvsearch",
    "amd", "bootstrap"
  )
  if(tool %in% search_tools && engine != "nonmem") {
    if(verbose) {
      cli::cli_alert_info(c(
        "Running {.val {tool}} against an nlmixr-format model.",
        i = "Pharmpy will dispatch each candidate fit via {.code pharmpy.tools.fit(esttool = 'nlmixr')}; ensure {.pkg pyreadr} (Python) and a working {.pkg nlmixr2} install are available to the Rscript that pharmpy invokes."
      ))
    }
    options[["esttool"]] <- "nlmixr"
  }

  ## Remove $TABLE records, if requested
  if(remove_tables) {
    if(verbose) cli::cli_alert_info("Removing $TABLE records from model")
    model <- remove_tables_from_model(model)
  }
  # Ensure residuals are outputted, when needed for the tool
  if(tool == "ruvsearch") {
    if(remove_tables) {
      cli::cli_alert_warning("`remove_tables` is set to `TRUE`, but `ruvsearch` requires a table with residuals to function. Adding back $TABLE record with residuals and basic fit info.")
    }
    model <- model |>
      add_default_output_tables("fit")
  }
  ## Check results, if needed rerun model
  req_results <- c("modelsearch", "covsearch", "iivsearch", "ruvsearch", "amd")
  if((is.null(results) && tool %in% req_results) || tool == "ruvsearch") {
    if(verbose)
      cli::cli_alert_info("No `results` provided, running the model first to generate `results` object.")
    results <- run_nlme(
      id = id,
      model = model,
      force = force
    )
  }

  ## For tools that require results, prefer the model stored in the results
  ## object. That model was re-read from the run folder by run_nlme() and
  ## therefore carries the correct file path needed for Pharmpy to resolve
  ## output tables (sdtab etc.) when running in a Dask worker.
  if(tool %in% req_results && !is.null(results)) {
    model_from_results <- attr(results, "model")
    if(!is.null(model_from_results)) {
      if(!is.null(model) && verbose) {
        cli::cli_alert_info("Using model stored in `results` to ensure file path consistency.")
      }
      model <- model_from_results
    }
  }

  ## Prepare run folder
  if(is.null(folder)) {
    folder <- getwd()
  }
  run_folder <- file.path(getwd(), id)
  if(!dir.exists(run_folder))
    run_folder <- create_run_folder(id, folder, force, verbose)

  ## Clean Pharmpy run folders, if requested
  clean_pharmpy_runfolders(id, folder, tool, remove = clean)

  ## Run tool
  if(verbose) {
    cli::cli_alert_info(
      paste0("Starting {tool} in ", run_folder)
    )
  }

  ## - simulation: ensure it is a simulation
  if(tool == "simulation") {
    if(verbose)
      cli::cli_alert_info("Making sure model is a simulation model")
    model <- model |>
      pharmr::set_simulation(n = PKPDsim::ifelse0(options$n, 1)) |>
      pharmr::set_name("sim")
    options$n <- NULL
  }

  ## Pharmpy 2.0 regression workaround: run_iivsearch crashes when
  ## search_space=None (the documented default) because
  ## ModelFeatures.create(None) raises TypeError. Inject a sensible
  ## default so the call works out of the box.
  if(tool == "iivsearch" && is.null(options$search_space)) {
    options$search_space <- "IIV(@PK,EXP)"
    if(verbose)
      cli::cli_alert_info(
        "No `search_space` provided; defaulting to {.val IIV(@PK,EXP)}"
      )
  }

  ## MFL case-sensitivity workaround (pharmpy/pharmpy#4576): the MFL parser
  ## uppercases all identifiers in `search_space`, then validates them
  ## case-sensitively against `datainfo.names`. To make lowercase column
  ## conventions work, warn on lowercase identifiers in the search_space and
  ## uppercase both the search_space string and the model's columns. Only
  ## applied when an MFL `search_space` is actually being passed, to avoid
  ## mutating column names for tools that don't consume MFL (bootstrap,
  ## simulation, etc.).
  if(uppercase_mfl && !is.null(options$search_space)) {
    lowercase_ids <- detect_lowercase_identifiers(options$search_space)
    if(length(lowercase_ids) > 0 && verbose) {
      cli::cli_alert_warning(c(
        "{.code search_space} contains lowercase identifier{?s}: {.val {lowercase_ids}}.",
        i = "Pharmpy's MFL parser uppercases all identifiers (see {.url https://github.com/pharmpy/pharmpy/issues/4576}); uppercasing now."
      ))
    }
    options$search_space <- toupper(options$search_space)
    renamed <- uppercase_model_columns(model)
    if(length(renamed$renamed) > 0) {
      if(verbose) {
        cli::cli_alert_info(c(
          "Uppercased {length(renamed$renamed)} model column{?s} to match MFL parser: {.val {paste0(names(renamed$renamed), ' -> ', unname(renamed$renamed))}}"
        ))
      }
      model <- renamed$model
    }
  }

  ## prepare arguments for call
  args <- c(
    list(model = model),
    options
  )
  if(tool %in% req_results) {
    args$results <- results
  }

  ## temporary rename of resamples argument
  ## TODO: remove after move to Pharmpy 2.0
  if(tool == "bootstrap") { 
    v_pharmpy <- stringr::str_split(as.character(packageVersion("pharmr")), "\\.")[[1]] |> 
      as.numeric()
    if(v_pharmpy[1] < 2) {
      if(!is.null(args$samples)) {
        args$resamples <- args$samples
        args$samples <- NULL
      }
    }
  }
  
  ## make the call to the Pharmpy tool
  tryCatch({
    withr::with_dir(run_folder, {
      res <- do.call(
        paste0("run_", tool),
        envir = asNamespace("pharmr"),
        args = args
      )
    })
  }, error = function(e) {
    cli::cli_abort("Pharmpy error running {tool}: {e}")
  })
  if(is.null(res)) {
    cli::cli_abort("Pharmpy error running {tool}.")
  }

  ## Post-processing, tool-specific
  ## Save final model to file, and attach to output object
  if(stringr::str_detect(tool, "(.*search|amd)")) {
    final_model <- update_parameters(res$final_model, res$final_results)
    final_model_code <- final_model$code
    writeLines(
      final_model_code,
      file.path(run_folder, glue::glue("final_{tool}.mod"))
    )
    attr(res, "final_model") <- final_model
  }
  if(tool == "simulation") {
    pharmpy_runfolders <- get_pharmpy_runfolders(
      id = id,
      folder = folder,
      tool = tool
    )
    full_table_path <- file.path(run_folder, utils::tail(pharmpy_runfolders, 1), "models", "sim")
    tables <- get_tables_from_fit(
      model,
      path = full_table_path
    )
    if(verbose) {
      if(length(tables) > 0) {
        cli::cli_alert_info(paste0("Attaching {length(tables)} table", ifelse(length(tables) > 1, "s", ""), " from {tool} to output"))
      } else {
        cli::cli_alert_info("No tables found from {tool} at {full_run_path}")
      }
      attr(res, "tables") <- tables
    }
  }

  res

}
