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
#' https://pharmpy.github.io/latest/mfl.html. For `tool = "structsearch"` with
#' `type = "tmdd"`, the pharmr.extra-specific `kd` element (target dissociation
#' constant seed, in concentration units) seeds the QSS candidates' target
#' parameters via [seed_tmdd_results()] and is *not* forwarded to Pharmpy.
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
  req_results <- c("modelsearch", "covsearch", "iivsearch", "ruvsearch", "amd", "structsearch")
  ## Tools that only support NONMEM-format models. Pharmpy has no (or immature)
  ## nlmixr dispatch for these, and they are absent from `search_tools` below,
  ## so the nlmixr code paths (esttool injection, report suppression) never run
  ## for them. Abort early rather than let them fail deep inside Pharmpy.
  nonmem_only_tools <- c("ruvsearch", "structsearch")
  if(tool %in% nonmem_only_tools && engine != "nonmem") {
    cli::cli_abort(c(
      "Pharmpy {.val {tool}} does not currently support nlmixr-format models.",
      i = "Use a NONMEM-format model for {.val {tool}}."
    ))
  }
  needs_native_nlmixr_results <- tool %in% req_results && engine != "nonmem"
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
  if(((is.null(results) && tool %in% req_results) || tool == "ruvsearch") &&
     !needs_native_nlmixr_results) {
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

  ## TMDD structsearch: seed the target-binding parameters. Pharmpy's
  ## `create_qss_models()` derives sane initial estimates for POP_KDC (= POP_KM)
  ## and POP_KINT (= POP_KM * POP_CLMM / (R_0 * VC)) *only* when the base-fit
  ## results carry POP_KM and POP_CLMM. A plain linear PK base fit has neither,
  ## so every QSS candidate starts from POP_KINT = POP_KDC = 0.1 and the stiff
  ## TMDD ODE diverges (PRED EXIT), which cascades into a Pharmpy
  ## `KeyError('POP_KINT')`. Passing `kd` augments the results with POP_KM (= kd,
  ## the target dissociation constant, in concentration units) and POP_CLMM
  ## (= the fitted POP_CL) so the search starts from a physiologically-plausible
  ## point. Ignored for non-TMDD tools or when the base is already MM-parameterised.
  if(tool == "structsearch" && identical(options$type, "tmdd") &&
     !is.null(options$kd) && !is.null(results)) {
    results <- seed_tmdd_results(results, kd = options$kd, verbose = verbose)
  } else if(!is.null(options$kd)) {
    cli::cli_alert_warning(
      "Ignoring {.code kd}: it only applies to {.code tool = \"structsearch\"} with \\
       {.code options$type = \"tmdd\"} and non-NULL {.code results}. No seeding performed."
    )
  }
  options$kd <- NULL  # `kd` is a pharmr.extra convenience, not a run_structsearch arg

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

  if(needs_native_nlmixr_results &&
     !inherits(results, "pharmpy.workflows.results.ModelfitResults")) {
    if(verbose) {
      cli::cli_alert_info(c(
        "Converting nlmixr modelfit results to Pharmpy-native results for {.val {tool}}.",
        i = "This avoids refitting the input model before running the Pharmpy search tool."
      ))
    }
    native_results <- tryCatch(
      as_native_pharmpy_modelfit_results(results),
      error = function(e) {
        attr(e, "pharmr_extra_native_conversion_failed") <- TRUE
        e
      }
    )
    if(inherits(native_results, "error")) {
      if(verbose) {
        cli::cli_alert_warning(c(
          "Could not convert nlmixr results to Pharmpy-native results; refitting input model.",
          i = conditionMessage(native_results)
        ))
      }
      native_results <- NULL
    }
    if(!is.null(native_results)) {
      results <- native_results
    } else {
      if(verbose) {
        cli::cli_alert_info(c(
          "Generating Pharmpy-native nlmixr modelfit results for {.val {tool}}.",
          i = "The R-shaped {.fn run_nlme} result cannot be passed to Pharmpy search tools directly."
        ))
      }
      results <- withr::with_dir(run_folder, {
        pharmr::fit(model, esttool = "nlmixr", name = "input", ncores = 1)
      })
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
  restore_report_available <- NULL
  if(tool %in% search_tools && engine != "nonmem") {
    restore_report_available <- suppress_pharmpy_reports()
    on.exit(restore_report_available(), add = TRUE)
  }

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

suppress_pharmpy_reports <- function() {
  reticulate::py_run_string("
import pharmpy.tools.reporting as _pharmr_extra_reporting
if not hasattr(_pharmr_extra_reporting, '_pharmr_extra_report_available'):
    _pharmr_extra_reporting._pharmr_extra_report_available = _pharmr_extra_reporting.report_available
_pharmr_extra_reporting.report_available = lambda results: False
")
  function() {
    reticulate::py_run_string("
import pharmpy.tools.reporting as _pharmr_extra_reporting
if hasattr(_pharmr_extra_reporting, '_pharmr_extra_report_available'):
    _pharmr_extra_reporting.report_available = _pharmr_extra_reporting._pharmr_extra_report_available
    delattr(_pharmr_extra_reporting, '_pharmr_extra_report_available')
")
  }
}

#' Seed TMDD target-binding parameters into a base-fit results object
#'
#' Returns a copy of `results` whose `parameter_estimates` gains `POP_KM` (= `kd`)
#' and `POP_CLMM` (= the fitted `POP_CL`). Pharmpy's TMDD `structsearch` uses these
#' to seed the QSS candidates' `POP_KDC` / `POP_KINT`; without them the stiff TMDD
#' ODE diverges. Used internally by [call_pharmpy_tool()] when `tool = "structsearch"`,
#' `options$type = "tmdd"` and `options$kd` is supplied. If the results already carry
#' `POP_KM`/`POP_CLMM` (an MM-parameterised base) the object is returned unchanged.
#'
#' @param results a Pharmpy `ModelfitResults` object (e.g. from [run_nlme()]).
#' @param kd target dissociation constant seed (concentration units).
#' @param verbose emit an info message?
#'
#' @return a `ModelfitResults` with augmented `parameter_estimates`.
#' @keywords internal
seed_tmdd_results <- function(results, kd, verbose = TRUE) {
  pe <- results$parameter_estimates
  nm <- names(pe)
  ## For NONMEM `ModelfitResults`, `parameter_estimates` may be an unconverted
  ## pandas Series (names() == NULL); convert so we can index it by name. See
  ## the same fallback in run_sim.R.
  if(is.null(nm) && inherits(pe, "python.builtin.object")) {
    pe <- reticulate::py_to_r(pe)
    nm <- names(pe)
  }
  if(all(c("POP_KM", "POP_CLMM") %in% nm)) {
    return(results)
  }
  if(!"POP_CL" %in% nm) {
    cli::cli_alert_warning(
      "Cannot seed TMDD parameters: base results have no {.val POP_CL}. Proceeding without seeding."
    )
    return(results)
  }
  ## Only add the parameters that are missing, so a partial-MM base fit keeps
  ## its already-fitted POP_KM / POP_CLMM instead of having them overwritten by
  ## the raw seed (a duplicate index would silently clobber the fitted value).
  new_vals <- stats::setNames(as.numeric(pe), nm)
  if(!"POP_KM" %in% nm) {
    new_vals <- c(new_vals, POP_KM = as.numeric(kd))
  }
  if(!"POP_CLMM" %in% nm) {
    new_vals <- c(new_vals, POP_CLMM = as.numeric(pe[["POP_CL"]]))
  }
  pd <- reticulate::import("pandas", convert = FALSE)
  dc <- reticulate::import("dataclasses", convert = FALSE)
  ser <- pd$Series(as.list(new_vals))
  if(verbose) {
    cli::cli_alert_info(
      "Seeding TMDD target parameters for structsearch ({.code POP_KM = {kd}}, {.code POP_CLMM = POP_CL})."
    )
  }
  dc$replace(results, parameter_estimates = ser)
}
