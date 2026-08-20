#' Run simulations
#'
#' @inheritParams run_nlme
#' @param id base run id (default a random `sim_*`). Each regimen is run in its
#' own subfolder `id/regimen_<i>` (`<i>` = 1-based regimen index), so regimens
#' don't overwrite each other's output.
#' @param model either a Pharmpy model object, or a filename (for a model
#' with NONMEM model code). If the latter, `run_sim()` will attempt to load the
#' model into Pharmpy first.
#' @param fit a Pharmpy modelfit object.
#' @param data a NONMEM-format data.frame to use as the simulation dataset.
#' Typically the output of [create_sim_dataset()]. If `NULL`, the dataset
#' attached to `model` is used as-is.
#' @param n_iterations number of iterations of the entire simulation to
#' perform. The dataset for the simulation will stay the same between each
#' iterations.
#' @param n_uncertainty number of parameter sets to draw from the fit's
#' covariance matrix to propagate parameter uncertainty. If `NULL` (default)
#' or `0`, the point estimates are used and no uncertainty is propagated. If a
#' positive integer, the point estimate is omitted and `n_uncertainty`
#' parameter sets are sampled instead; one simulation is run per draw with its
#' thetas/omegas/sigmas updated, so a total of `n_iterations * n_uncertainty`
#' simulations are performed. Requires a `fit` object carrying a covariance
#' matrix (i.e. the model was run with a `$COVARIANCE` step or SIR). When set,
#' the output gains a `.uncertainty` column counting the replicate (1-based).
#'
#' Only parameters present in the covariance matrix are resampled; any other
#' estimated parameters are held at their point estimates and a warning lists
#' them. This matters for nlmixr2 fits in particular: the default nlmixr2
#' covariance step reports uncertainty only for the population fixed effects,
#' so residual and random-effect variance parameters (SIGMA, OMEGA/IIV) are
#' held fixed. For full uncertainty on those, use a bootstrap
#' (`nlmixr2est::bootstrapFit()`). NONMEM `$COVARIANCE` typically covers all
#' parameters, so all are resampled.
#'
#' This is the same idea as NONMEM's own `$PRIOR NWPRI` +
#' `$SIMULATION ... TRUE=PRIOR`, and the two are checked against each other in
#' `tests/testthat/test-run_sim-nwpri.R`. Aggregates agree closely: over 1000
#' draws from the same fit, means and standard deviations of the fixed effects
#' match to within 0.3% and 3%, those of the variance parameters to within 5%
#' and 8%, and the resulting 90% uncertainty interval on the predicted profile
#' to within 7%. Two differences are structural rather than numerical: NWPRI
#' draws OMEGA and SIGMA from (right-skewed) inverse-Wishart distributions
#' whereas the draws here come from a single truncated multivariate normal, and
#' NWPRI treats the THETA, OMEGA and SIGMA priors as independent blocks whereas
#' the draws here keep the THETA-OMEGA and THETA-SIGMA covariances that
#' `$COVARIANCE` reports.
#' @param add_pk_variables calculate basic PK variables: CMAX_OBS, TMAX_OBS,
#' CMIN_OBS, and (when `CL` is in the output table) AUC_SS. AUC_SS is derived
#' as the last dose in the simulation dataset divided by CL.
#' @param update_table should any existing $TABLE records be removed, and a new
#'  `simtab` be created? This is default. If `FALSE`, it will leave $TABLEs as
#' specifed in the model. However, in the return object, only the first table
#' is returned back. If `FALSE`, the `add_pk_variables` argument will be ignored.
#' @param tool the tool to run the model in, either `nonmem`, or `nlmixr`.
#' @param variables vector of variables to output. If `NULL`, will output
#' default variables `c("ID", "TIME", "DV", "EVID", "PRED")` as well as
#' all variables declared in the NONMEM code.
#' @param path folder in which to create the run folder(s). Each regimen is
#' run in its own subfolder `id/regimen_<i>`. If `NULL` (default), the folder
#' is forwarded to [run_nlme()] unset, so `run_nlme()`'s own default applies.
#' @param output_file TODO
#' @param seed TODO
#' @param n_cores number of processes to run uncertainty replicates on
#' (default `1`, i.e. sequential; unchanged behaviour). Values `> 1` spread the
#' `n_uncertainty` replicates over that many worker processes and are
#' supported for the `nlmixr2` tool only; a NONMEM run warns and falls back to
#' sequential. Output is identical to a sequential run for the same `seed`,
#' since each replicate keeps its own derived seed (`seed + r`) and results are
#' reassembled by replicate index. Ignored when no uncertainty is requested.
#' The machine's cores are divided over the workers (rxode2's solver threads
#' are capped per worker), so raising `n_cores` does not oversubscribe the CPU.
#'
#' @returns data.frame with simulation results. When `n_uncertainty` is used,
#' the result also carries `n_uncertainty_requested` and `n_uncertainty_kept`
#' attributes: replicates that fail on the nlmixr2 backend are dropped with a
#' warning, so these let a caller detect a short (and potentially biased) set
#' of draws without parsing warnings. On the NONMEM backend a failing replicate
#' aborts the run instead.
#'
#' @export
run_sim <- function(
    fit = NULL,
    data = NULL,
    model = NULL,
    id = irxutils::get_random_id("sim_"),
    path = NULL,
    force = FALSE,
    tool = c("auto", "nonmem", "nlmixr2"),
    n_iterations = 1,
    n_uncertainty = NULL,
    variables = NULL,
    add_pk_variables = FALSE,
    output_file = "simtab",
    update_table = TRUE,
    seed = 12345,
    verbose = TRUE,
    ## New arguments go last: `verbose` was the final positional argument
    ## before, and callers passing it positionally would otherwise silently
    ## set `n_cores` instead.
    n_cores = 1
) {

  ## parse arguments
  if(is.null(fit) && is.null(model)) {
    cli::cli_abort("For simulations we need either a `fit` object, or a `model` file (with updated estimates)")
  }
  if(is.null(model)) {
    if(!is.null(attr(fit, "final_model"))) {
      model <- attr(fit, "final_model")
    } else {
      cli::cli_abort("No proper model object available. Need either a `model` object or a `fit` object with a model attached.")
    }
  } else {
    if(inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_alert_info("Supplied `model` is a Pharmpy model object.")
    } else {
      cli::cli_alert_info("Supplied `model` is not a Pharmpy model object. Trying to load in Pharmpy.")
      if(!is.null(data) && inherits(data, "data.frame")) {
        data_file <- tempfile(fileext = ".csv")
        write.csv(data, data_file, quote = F, row.names = F)
        model <- create_model_from_file(model_file = model, data = data_file)
      } else {
        model <- create_model_from_file(model_file = model)
      }
      if(inherits(model, "pharmpy.model.model.Model")) {
        cli::cli_alert_info("Model successfully imported as Pharmpy model object.")
      } else {
        cli::cli_abort("Could not load model into Pharmpy. Please check supplied model object or model code.")
      }
    }
  }
  tool <- match.arg(tool)
  if(tool == "auto") {
    tool <- get_tool_from_model(model)
    if(tool == "nlmixr") tool <- "nlmixr2"
  }
  if(! tool %in% c("nonmem", "nlmixr2")) {
    cli::cli_abort("Unsupported simulation tool: {tool}.")
  }

  ## Check `data` here rather than only inside the engine: parallel uncertainty
  ## replicates never reach the engine in this process, so an unusable `data`
  ## would otherwise surface as N worker failures instead of one clear message.
  validate_sim_data(data)

  ## Validate uncertainty request. Treat NULL/0 as "no uncertainty" (point
  ## estimate). Sampling from the covariance matrix needs a `fit` object; a
  ## bare model carries no covariance.
  if(!is.null(n_uncertainty)) {
    n_num <- suppressWarnings(as.numeric(n_uncertainty))
    ## Reject (rather than silently truncate) fractional / non-numeric input so
    ## e.g. `0.5` does not quietly collapse to a point-estimate run. Bound above
    ## by the integer range too: `as.integer()` returns NA past that, which would
    ## later blow up in `seq_len()`.
    if(length(n_num) != 1 || is.na(n_num) || n_num < 0 ||
       n_num != round(n_num) || n_num > .Machine$integer.max) {
      cli::cli_abort("`n_uncertainty` must be a non-negative integer (<= {(.Machine$integer.max)}) or NULL.")
    }
    n_uncertainty <- as.integer(n_num)
    if(n_uncertainty == 0) n_uncertainty <- NULL
  }
  if(!is.null(n_uncertainty)) {
    if(is.null(fit) || is.null(fit$covariance_matrix) ||
       is.null(fit$parameter_estimates)) {
      cli::cli_abort(c(
        "`n_uncertainty` simulation needs a `fit` object with a covariance matrix.",
        i = "Run the model with a {.code $COVARIANCE} step (or SIR) so parameter uncertainty can be sampled."
      ))
    }
  }

  ## Engine: run one full simulation (all regimens, `n_iterations` subproblems)
  ## for a given model and seed. Captures the remaining arguments lexically.
  ## `model`/`seed` vary between uncertainty replicates.
  run_sim_engine <- function(model, seed, verbose = TRUE) {
  input_data <- model$dataset

  ## Engine dispatch: nlmixr2 simulations go through rxode2::rxSolve()
  ## directly. Pharmpy-driven nlmixr simulation needs the same pyreadr
  ## dependency that blocks the fitter, and rxSolve avoids the round-trip.
  ## Parallel uncertainty replicates bypass this engine and call
  ## run_sim_nlmixr() from a worker (see make_nlmixr_replicate_fn()), so keep
  ## the two argument lists in step.
  if(tool == "nlmixr2") {
    return(run_sim_nlmixr(
      fit = fit,
      data = data,
      model = model,
      id = id,
      path = path,
      n_iterations = n_iterations,
      variables = variables,
      add_pk_variables = add_pk_variables,
      output_file = output_file,
      seed = seed,
      verbose = verbose
    ))
  }

  ## Use provided data or fall back to model's dataset
  if(is.null(data)) {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- as.data.frame(input_data)
    sim_data[[".regimen"]] <- "original regimens"
  } else {
    validate_sim_data(data)
    sim_data <- data
    if(!".regimen" %in% names(sim_data)) {
      sim_data[[".regimen"]] <- "original regimens"
    }
  }

  ## get unique regimens / datasets to simulate
  unique_regimens <- unique(sim_data[[".regimen"]])
  comb <- list()

  ## Loop over regimens to simulate. Each regimen gets its own run folder
  ## (`id/regimen_<i>`) so regimens don't overwrite each other's output.
  ## Numeric indexing avoids sanitizing user labels that may contain spaces
  ## NONMEM cannot handle in `$DATA` paths.
  for(i in seq_along(unique_regimens)) {
    reg_label <- unique_regimens[i]
    id_i <- file.path(id, paste0("regimen_", i))

    ## grab data for regimen
    sim_data_regimen <- sim_data |>
      dplyr::filter(.data$.regimen == reg_label) |>
      dplyr::select(-".regimen")
    if("EVID" %in% names(sim_data_regimen)) {
      sim_data_regimen <- sim_data_regimen |>
        dplyr::arrange(.data$ID, .data$TIME, -.data$EVID)
    } else {
      sim_data_regimen <- sim_data_regimen |>
        dplyr::arrange(.data$ID, .data$TIME)
    }
    
    ## Ensure column names & order matches
    if(all(names(sim_data_regimen) %in% names(input_data))) {
      sim_data_regimen <- sim_data_regimen[, names(input_data)]
    }

    ## Set simulation (pharmr::set_simulation() modifies the model that sometimes invalidate the model, so add manually)
    if(verbose) cli::cli_alert_info("Changing model to simulation-only model")
    sim_model <- model |>
      set_simulation_clean(seed = seed, n = n_iterations)

    ## Add tables
    if(update_table) {
      if(verbose) cli::cli_alert_info("Updating table record(s)")
      parameter_names <- get_defined_pk_parameters(sim_model)
      if(is.null(variables)) {
        default_variables <- c("ID", "TIME", "DV", "EVID", "PRED")
        covariate_names <- vapply(
          pharmr::get_model_covariates(sim_model),
          function(x) x$name,
          character(1)
        )
        variables <- c(default_variables, get_declared_variables(sim_model), covariate_names)
      }
      checked_variables <- c()
      for(variab in variables) {
        check_var <- check_nm_table_variables(sim_model, variab, throw_error = FALSE)
        if(is.null(check_var)) { # i.e. IPRED is declared as variable and we can safely add to table
          checked_variables <- c(checked_variables, variab)
        }
      }
      table_variables <- unique(c(checked_variables, parameter_names))
      sim_model <- sim_model |>
        remove_tables_from_model(reload_dataset = FALSE) |>
        add_table_to_model(table_variables, file = output_file, reload_dataset = FALSE)
    } else {
      if(verbose) cli::cli_alert_info("Using existing table record(s)")
    }

    ## Update dataset (in safe way, avoiding pharmr::set_dataset)
    if(verbose) cli::cli_alert_info("Updating dataset reference")
    new_dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(sim_data_regimen, new_dataset_file, quote = F, row.names = F)
    
    ## Run simulation
    if(verbose) cli::cli_alert_info("Running simulation ({reg_label})")

    ## sim_data_regimen. Forward `path` only when set, so run_nlme()'s own
    ## default applies otherwise (decoupled from any getwd() default here).
    nlme_args <- list(
      model = sim_model,
      data = new_dataset_file,
      id = id_i,
      force = TRUE,
      copy_dataset = TRUE,
      auto_stack_encounters = FALSE,
      verbose = FALSE
    )
    if(!is.null(path)) nlme_args$path <- path
    results <- do.call(run_nlme, nlme_args)

    ## Detect silent NONMEM failures: pharmpy/run_nlme do not raise when a
    ## simulation produces no output table, so we check here and surface the
    ## .lst error before the next regimen overwrites the run folder.
    res_tables <- attr(results, "tables")
    sim_tab <- if(length(res_tables) > 0) res_tables[[1]] else NULL
    if(is.null(sim_tab) || nrow(sim_tab) == 0) {
      abort_on_failed_sim(
        regimen_label = reg_label,
        fit_folder = attr(results, "fit_folder") %||%
          file.path(path %||% getwd(), id_i)
      )
    }

    ## post-processing
    if(update_table) {
      if(add_pk_variables) {
        ## Derive the dosing regimen from sim_data_regimen so AUC_SS can be
        ## computed in calc_pk_variables (needs regimen$dose).
        regimen_for_pk <- NULL
        if("EVID" %in% names(sim_data_regimen) && "AMT" %in% names(sim_data_regimen)) {
          dose_rows <- sim_data_regimen[sim_data_regimen$EVID == 1, , drop = FALSE]
          if(nrow(dose_rows) > 0) {
            regimen_for_pk <- list(dose = dose_rows$AMT)
          }
        }
        attr(results, "tables")[[output_file]] <- calc_pk_variables(
          data = attr(results, "tables")[[output_file]],
          regimen = regimen_for_pk
        )
      }
    }

    ## grab table, return
    if(verbose) cli::cli_alert_info("Exporting simulation results ({reg_label})")
    comb[[reg_label]] <- attr(results, "tables")

  }

  ## combine back down to single data.frame again
  out <- lapply(unique_regimens, function(reg_label) {
    table_names <- names(comb[[reg_label]])
    simtab <- table_names[1]
    if(!is.null(simtab) && !is.null(comb[[reg_label]][[simtab]])) {
      return(
        comb[[reg_label]][[simtab]] |>
          dplyr::mutate(regimen_label = reg_label)
      )
    } else {
      cli::cli_warn("Simulation for {reg_label} did not output any results.")
      return(data.frame())
    }
  }) |>
    dplyr::bind_rows()
  
  if(verbose) cli::cli_alert_success("Done")
  out
  } ## end run_sim_engine

  ## No uncertainty: single pass with point estimates (unchanged behaviour)
  if(is.null(n_uncertainty)) {
    return(run_sim_engine(model, seed, verbose = verbose))
  }

  ## Uncertainty: draw `n_uncertainty` parameter sets from the covariance
  ## matrix and run one simulation per draw, each with updated
  ## thetas/omegas/sigmas and tagged with a 1-based `.uncertainty` index.
  ## The point estimate is intentionally omitted.
  if(verbose) {
    cli::cli_alert_info("Sampling {n_uncertainty} parameter set{?s} from covariance matrix")
  }
  draws <- sample_uncertainty_parameters(
    model = model,
    parameter_estimates = fit$parameter_estimates,
    covariance_matrix = fit$covariance_matrix,
    n = n_uncertainty,
    seed = seed
  )

  ## Warn (once, regardless of `verbose`) about estimated parameters the
  ## covariance matrix does not cover: these are held at their point estimates,
  ## so their uncertainty is not propagated. Common for nlmixr2 fits, whose
  ## default covariance step omits variance parameters (SIGMA, OMEGA/IIV).
  pe_names <- names(fit$parameter_estimates)
  if(is.null(pe_names) && inherits(fit$parameter_estimates, "python.builtin.object")) {
    pe_names <- names(reticulate::py_to_r(fit$parameter_estimates))
  }
  held_fixed <- setdiff(pe_names, names(draws))
  if(length(held_fixed) > 0) {
    cli::cli_warn(c(
      "!" = "Covariance matrix does not cover all estimated parameters; \\
             {length(held_fixed)} parameter{?s} held at point estimate{?s} \\
             (uncertainty not propagated).",
      "i" = "Held fixed: {held_fixed}",
      "i" = "nlmixr2 fits often omit variance parameters (SIGMA, OMEGA/IIV) \\
             from the covariance step; use {.fn nlmixr2est::bootstrapFit} for \\
             full uncertainty on those."
    ))
  }

  ## Replicates are independent (own draw, own seed, combined only at the end),
  ## so they can be spread over worker processes. Only the nlmixr2/rxode2
  ## backend is parallelised: the NONMEM backend drives Pharmpy through Python
  ## (not sendable to a worker) and writes per-regimen run folders that
  ## concurrent replicates would clobber.
  n_cores <- resolve_n_cores(n_cores)
  if(n_cores > 1L && tool != "nlmixr2") {
    cli::cli_warn(c(
      "Parallel uncertainty replicates are supported for the {.val nlmixr2} tool only.",
      i = "Running the {n_uncertainty} replicate{?s} sequentially."
    ))
    n_cores <- 1L
  }

  if(n_cores > 1L) {
    ## Render every replicate's model here, in the parent: applying a draw is a
    ## Pharmpy (Python) operation and the resulting model object cannot cross a
    ## process boundary, but the nlmixr2 code it renders to (a string) can.
    ## Regenerated rather than read from the cached `nlmixr_code` attribute,
    ## which still holds the point estimates.
    if(verbose) {
      cli::cli_alert_info("Preparing {n_uncertainty} replicate model{?s}")
    }
    specs <- lapply(seq_len(n_uncertainty), function(r) {
      m <- pharmr::set_initial_estimates(
        model, inits = as.list(draws[r, , drop = FALSE])
      )
      list(index = r, code = make_nlmixr_saem_safe(m$code), seed = seed + r)
    })
    ## Resolve the dataset in the parent for the same reason (it is identical
    ## across replicates, so this also avoids re-reading it per worker).
    replicate_fn <- make_nlmixr_replicate_fn(
      data = data %||% as.data.frame(model$dataset),
      n_iterations = n_iterations,
      variables = variables,
      add_pk_variables = add_pk_variables,
      output_file = output_file
    )
    if(verbose) {
      cli::cli_alert_info(
        "Running {n_uncertainty} uncertainty replicate{?s} on {n_cores} core{?s}"
      )
    }
    replicates <- parallel_lapply(specs, replicate_fn, n_cores = n_cores)
  } else {
    pb <- NULL
    if(verbose) {
      pb <- cli::cli_progress_bar(
        "Uncertainty replicates", total = n_uncertainty, .envir = environment()
      )
    }
    replicates <- lapply(seq_len(n_uncertainty), function(r) {
      res <- run_captured(r, function() {
        inits <- as.list(draws[r, , drop = FALSE])
        m <- pharmr::set_initial_estimates(model, inits = inits)
        ## Force nlmixr2 code to regenerate from the updated estimates: a stale
        ## cached `nlmixr_code` attribute would otherwise make run_sim_nlmixr()
        ## silently simulate the point estimates on every replicate.
        attr(m, "nlmixr_code") <- NULL
        ## distinct seed per draw so subproblem RNG differs between replicates.
        ## `verbose = FALSE` + `suppressMessages()`: the engine's per-regimen
        ## alerts would tear down and redraw the progress bar on every
        ## replicate, and the worker path silences them the same way.
        suppressMessages(run_sim_engine(m, seed + r, verbose = FALSE))
      })
      if(tool == "nonmem" && inherits(res$result, "condition")) {
        ## Pre-parallel behaviour, kept deliberately: NONMEM replicate failures
        ## are typically systematic (licence, no output table, clobbered run
        ## folder), so carrying on would burn the remaining replicates only to
        ## return a silently truncated set of draws.
        if(!is.null(pb)) cli::cli_progress_done(id = pb)
        emit_replicate_warnings(r, res$warnings)
        cli::cli_abort("Uncertainty replicate {r} failed.", parent = res$result)
      }
      ## Emit as we go: a sequential run of a slow backend should not stay
      ## quiet about a misbehaving replicate until the last one has finished.
      emit_replicate_warnings(r, res$warnings)
      res$warnings <- list()
      if(!is.null(pb)) cli::cli_progress_update(id = pb)
      res
    })
    if(!is.null(pb)) cli::cli_progress_done(id = pb)
  }

  ## Assemble by replicate index (not by completion order) so `.uncertainty`
  ## stays 1-based and ordered, and re-emit whatever the replicates raised:
  ## worker processes have no console of their own.
  out <- lapply(replicates, function(repl) {
    emit_replicate_warnings(repl$index, repl$warnings)
    res <- repl$result
    if(inherits(res, "condition")) {
      ## Drop the failed replicate rather than aborting the whole run.
      msg <- conditionMessage(res)
      cli::cli_warn("Uncertainty replicate {repl$index} failed ({msg}); omitted.")
      return(NULL)
    }
    if(is.null(res) || nrow(res) == 0) {
      ## Surface dropped replicates so a short result set is not mistaken for a
      ## complete `1:n_uncertainty` run.
      cli::cli_warn(
        "Uncertainty replicate {repl$index} produced no simulation output; omitted."
      )
      return(NULL)
    }
    res[[".uncertainty"]] <- repl$index
    res
  }) |>
    dplyr::bind_rows()

  if(nrow(out) == 0) {
    cli::cli_abort(c(
      "All {n_uncertainty} uncertainty replicate{?s} failed; no simulation output.",
      i = "See the warnings above for the individual failures."
    ))
  }

  ## Record how many replicates actually survived, so callers can detect a
  ## short result set programmatically instead of having to scrape warnings.
  n_kept <- length(unique(out[[".uncertainty"]]))
  attr(out, "n_uncertainty_requested") <- n_uncertainty
  attr(out, "n_uncertainty_kept") <- n_kept
  if(n_kept < n_uncertainty) {
    cli::cli_warn(c(
      "Only {n_kept} of {n_uncertainty} uncertainty replicate{?s} produced output.",
      i = "Replicates that fail tend to be the extreme parameter draws, so \
           intervals computed over {.field .uncertainty} may be too narrow.",
      i = "The counts are on the result as the {.field n_uncertainty_kept} and \
           {.field n_uncertainty_requested} attributes."
    ))
  }
  if(verbose) {
    cli::cli_alert_success("Done ({n_kept}/{n_uncertainty} uncertainty replicate{?s})")
  }
  out
}

#' Check that a supplied simulation dataset is usable
#'
#' @param data candidate simulation dataset; `NULL` means "use the dataset
#' attached to the model" and is accepted.
#'
#' @returns `NULL`, invisibly. Called for its side effect of aborting.
#' @noRd
validate_sim_data <- function(data) {
  if(!is.null(data) && !inherits(data, "data.frame")) {
    cli::cli_abort(
      c("`data` must be a data.frame (typically the output of {.fn create_sim_dataset}).",
        x = "Got an object of class {.cls {class(data)}}.",
        i = "To build a simulation dataset from a file or model, use {.fn create_sim_dataset} first.")
    )
  }
  invisible(NULL)
}

#' Build the worker function for parallel nlmixr2 uncertainty replicates
#'
#' A factory rather than an inline closure: the closure is serialised to the
#' worker together with its enclosing environment, and `run_sim()`'s own frame
#' holds the Pharmpy `model`/`fit` (Python objects that must not be sent to a
#' worker). Closing over this factory's frame instead keeps only plain R data.
#'
#' @param data resolved simulation dataset (identical for every replicate).
#' @inheritParams run_sim
#'
#' @returns a function taking one replicate spec (`index`, `code`, `seed`) and
#' returning the `run_captured()` envelope for it.
#' @noRd
make_nlmixr_replicate_fn <- function(
    data,
    n_iterations,
    variables,
    add_pk_variables,
    output_file
) {
  force(data)
  force(n_iterations)
  force(variables)
  force(add_pk_variables)
  force(output_file)
  function(spec) {
    run_captured(spec$index, function() {
      suppressMessages(run_sim_nlmixr(
        data = data,
        model_code = spec$code,
        n_iterations = n_iterations,
        variables = variables,
        add_pk_variables = add_pk_variables,
        output_file = output_file,
        seed = spec$seed,
        verbose = FALSE
      ))
    })
  }
}

#' Sample parameter vectors from a fit's covariance matrix
#'
#' Draws `n` parameter sets from a multivariate normal defined by the fit's
#' point estimates (means) and covariance matrix, for propagating parameter
#' uncertainty into simulations.
#'
#' We call `pharmpy.modeling.sample_parameters_from_covariance_matrix()`
#' directly rather than [pharmr::sample_parameters_from_covariance_matrix()]:
#' the pharmr wrapper coerces `parameter_estimates` to a Python `dict`, but
#' pharmpy (>= 2) requires a `pd.Series` (it indexes on `.index`) and raises
#' `'dict' object has no attribute 'index'` otherwise.
#'
#' @param model a Pharmpy model object.
#' @param parameter_estimates named numeric vector (or pandas Series) of
#' parameter point estimates, used as sampling means.
#' @param covariance_matrix parameter uncertainty covariance matrix, as an R
#' matrix/data.frame (row/column names = parameter names) or a pandas
#' DataFrame.
#' @param n number of parameter sets to draw.
#' @param seed random seed.
#'
#' @returns a data.frame with one sampled parameter set per row and one column
#' per parameter (columns follow the covariance matrix's parameters).
#' @noRd
sample_uncertainty_parameters <- function(
    model,
    parameter_estimates,
    covariance_matrix,
    n,
    seed
) {
  pd   <- reticulate::import("pandas", convert = FALSE)
  pmod <- reticulate::import("pharmpy.modeling", convert = FALSE)

  ## Normalise inputs to plain R structures; they may arrive as pandas objects
  ## (via reticulate auto-conversion off) or as native R vectors/data.frames.
  to_r <- function(x) {
    if(inherits(x, "python.builtin.object")) reticulate::py_to_r(x) else x
  }
  pe        <- to_r(parameter_estimates)
  pe_names  <- names(pe)
  pe_vals   <- as.numeric(pe)
  if(is.null(pe_names)) {
    cli::cli_abort("`parameter_estimates` must be a named vector/Series of parameters.")
  }

  cov_mat <- as.matrix(to_r(covariance_matrix))
  storage.mode(cov_mat) <- "double"
  cov_names <- colnames(cov_mat)
  if(is.null(cov_names)) {
    cli::cli_abort("`covariance_matrix` must have parameter names as row/column names.")
  }
  ## Align rows to columns before stripping names for pharmpy. A pandas
  ## DataFrame round-tripped through py_to_r keeps column names but may drop
  ## matching row names; and an R matrix could arrive with rows in a different
  ## order than columns. Either would silently mislabel the covariance (rows
  ## carry one parameter's variance but get another's name), so mirror when
  ## row names are absent, reorder when they are a permutation, and abort when
  ## they disagree as a set.
  row_names <- rownames(cov_mat)
  if(is.null(row_names)) {
    rownames(cov_mat) <- cov_names
  } else if(!setequal(row_names, cov_names)) {
    cli::cli_abort("`covariance_matrix` row and column names must reference the same parameters.")
  } else if(!identical(row_names, cov_names)) {
    cov_mat <- cov_mat[cov_names, , drop = FALSE]
  }

  ## pharmpy requires the means and the covariance matrix to span the same
  ## parameters. A covariance matrix often covers only the parameters that were
  ## estimated with a standard error (e.g. fixed effects), so restrict the
  ## means to the covariance parameters. Unsampled parameters keep the model's
  ## current estimates.
  missing_means <- setdiff(cov_names, pe_names)
  if(length(missing_means) > 0) {
    cli::cli_abort(c(
      "Covariance matrix references parameters absent from `parameter_estimates`.",
      x = "Missing: {missing_means}"
    ))
  }
  pe_vals  <- pe_vals[match(cov_names, pe_names)]
  pe_names <- cov_names

  pe_s   <- pd$Series(reticulate::np_array(pe_vals), index = as.list(pe_names))
  cov_df <- pd$DataFrame(
    reticulate::np_array(cov_mat),
    index   = as.list(cov_names),
    columns = as.list(cov_names)
  )

  draws <- pmod$sample_parameters_from_covariance_matrix(
    model,
    pe_s,
    cov_df,
    n    = as.integer(n),
    seed = as.integer(seed)
  )
  draws <- as.data.frame(reticulate::py_to_r(draws))
  ## Drop the residual pandas index metadata py_to_r leaves behind, so the
  ## result is a plain R data.frame (two identical draws compare equal).
  attr(draws, "pandas.index") <- NULL
  rownames(draws) <- NULL
  draws
}

#' Calculate some basic PK variables from simulated or observed data
#'
#' @param data data.frame in NONMEM format
#' @param run_sim
#'
#' @returns data.frame
calc_pk_variables <- function(
    data,
    regimen = NULL
) {

  if(!is.null(data)) {
    ## Find cmax/tmax for each ID
    data <- data |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(CMAX_OBS = max(.data$DV)) |>
      dplyr::mutate(TMAX_OBS = .data$TIME[match(.data$CMAX_OBS[1], .data$DV)][1])

    ## Find Cmin for each ID, for last interval
    if(all(c("ID", "EVID") %in% names(data))) {
      tmp_data <- data |>
        dplyr::group_by(.data$ID) |>
        dplyr::mutate(.dose_id = cumsum(.data$EVID == 1))
      last_obs_dose_id <- tmp_data |>
        dplyr::filter(.data$EVID == 0) |>
        dplyr::pull(".dose_id") |>
        utils::tail(1)
      cmin_data <- tmp_data |>
        dplyr::mutate(.dose_cmin = max(c(1, last_obs_dose_id))) |> # last full interval (before last dose)
        dplyr::filter(.data$.dose_id == .data$.dose_cmin & .data$EVID == 0) |>
        dplyr::summarise(CMIN_OBS = min(.data$DV))
      data <- dplyr::left_join(data, cmin_data, by = "ID")      
    } else {
      cli::cli_alert_info("Skipping Cmin calculation, some required columns not in output data.")      
    }

    ## Add AUC_SS as CL/dose, if we're simulating a specific regimen
    if(!is.null(regimen) && "CL" %in% names(data)) {
      suppressWarnings(
        last_dose <- as.numeric(utils::tail(regimen$dose, 1))
      )
      if(!is.na(last_dose) && last_dose > 0) {
        data <- data |>
          dplyr::mutate(AUC_SS = last_dose / .data$CL)
      } else {
        cli::cli_warn("Could not calculate AUCss, last dose could not be identified.")
      }
    }
  }

  data
}

#' Create dosing records, given a specified regimen as a data frame with
#' potentially multiple regimens and varying dosing times / doses
#'
#' @param regimen TODO
#' @param data TODO
#' @param n_subjects TODO
#' @param advan TODO
#' 
create_dosing_records <- function(
    regimen,
    data,
    n_subjects,
    advan = NULL
) {
  ids <- unique(data$ID)
  if(length(ids) < n_subjects) {
    ids <- c(ids, max(ids) + 1:(n_subjects-length(ids)))
  }
  if(!is.null(regimen$regimen)) {
    unq_reg <- unique(regimen$regimen)
  } else {
    regimen$regimen <- "regimen 1"
    unq_reg <- "regimen 1"
  }
  ## logic for picking dosing compartments
  cmt_oral <- 1
  cmt_iv <- 2
  if(!is.null(advan)) {
    if(advan %in% c(1, 3, 11)) {
      cmt_iv <- 1
      if(any(regimen$route %in% c("oral", "im", "sc"))) {
        cli::cli_abort("The model structure does not support oral, im, or sc dosing, only iv.")
      }
    }
  }
  ## validate per columns early so we get a clear error before looping
  if (!is.null(regimen$per)) {
    per_cols <- unique(regimen$per[!is.na(regimen$per)])
    missing_per <- per_cols[!per_cols %in% names(data)]
    if (length(missing_per) > 0) {
      cli::cli_abort(
        "Column(s) specified in `per` not found in dataset: {missing_per}. \\
         Available columns: {paste(names(data), collapse = ', ')}"
      )
    }
  }
  dose <- data.frame(
    ID = 1,
    TIME = regimen$time,
    AMT = regimen$dose,
    EVID = 1,
    MDV = 1,
    DV = 0,
    CMT = 1,
    .regimen = regimen$regimen
  )
  if(is.null(regimen$t_inf)) regimen$t_inf <- 0
  dose$RATE <- 0
  dose$RATE[regimen$t_inf != 0] <- dose$AMT[regimen$t_inf != 0] / regimen$t_inf[regimen$t_inf != 0]
  dose <- dose |>
    dplyr::mutate(CMT = dplyr::case_when(
      regimen$route %in% c("oral", "sc", "im") ~ cmt_oral, # logic for picking dosing cmt
      regimen$route %in% c("iv", "bolus", "infusion") ~ cmt_iv,
      .default = 1
    ))
  dose_df <- lapply(1:n_subjects, function(i) {
    d <- dose |>
      dplyr::mutate(ID = ids[i])
    ## scale AMT (and RATE) by per-subject covariate when `per` is specified
    if (!is.null(regimen$per)) {
      per_vals <- regimen$per
      non_na <- !is.na(per_vals)
      if (any(non_na)) {
        for (col in unique(per_vals[non_na])) {
          subj_vals <- data[data$ID == ids[i], col, drop = TRUE]
          cov_val <- subj_vals[!is.na(subj_vals)][1]
          rows <- non_na & per_vals == col
          d$AMT[rows] <- d$AMT[rows] * cov_val
          ## recompute RATE for any infusion rows that were scaled
          inf_rows <- rows & regimen$t_inf != 0
          if (any(inf_rows)) {
            d$RATE[inf_rows] <- d$AMT[inf_rows] / regimen$t_inf[inf_rows]
          }
        }
      }
    }
    d
  }) |>
    dplyr::bind_rows()
  dose_df
}

#' Create observation records, given a specified t_obs vector
#'
#' @param data TODO
#' @param t_obs TODO
#' @param n_subjects TODO
create_obs_records <- function(
    data,
    t_obs,
    n_subjects,
    model
) {
  ids <- unique(data$ID)
  if(length(ids) < n_subjects) {
    ids <- c(ids, max(ids) + 1:(n_subjects-length(ids)))
  }
  unq_reg <- unique(data[[".regimen"]])
  ## create a template row
  ## first try pull CMT from data. if not available in data, try based on ADVAN
  if("MDV" %in% names(data)) {
    cmt <- data |>
      dplyr::filter(.data$ID == 1 & .data$EVID == 0 & .data$MDV == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(CMT)
  } else {
    cmt <- data |>
      dplyr::filter(.data$ID == 1 & .data$EVID == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(CMT)
  }
  if(is.null(cmt) || is.na(cmt) || length(cmt) == 0) {
    cmt <- get_obs_compartment(model)
  }
  obs <- data.frame(
    ID = 1,
    TIME = t_obs,
    AMT = 0,
    EVID = 0,
    MDV = 0,
    DV = 0,
    CMT = cmt,
    RATE = 0
  )
  ## extend single sampling design to multiple subjects
  obs_df <- lapply(1:n_subjects, function(i) {
    obs |>
      dplyr::mutate(ID = ids[i])
  }) |>
    dplyr::bind_rows()
  ## extend to multiple regimens, if needed
  obs_df <- lapply(1:length(unq_reg), function(i) {
    obs_df |>
      dplyr::mutate(.regimen = unq_reg[i])
  }) |>
    dplyr::bind_rows()
  obs_df
}

match_type <- function(x, reference, cols = c("AMT", "RATE", "DV")) {
  for(key in cols) {
    if(inherits(reference[[key]], "character")) {
      x[[key]] <- as.character(x[[key]])
    }
  }
  x
}

fill_missing <- function(x, default = NA) {
  if (all(is.na(x))) {
    if(inherits(x, "character")) {
      rep(".", length(x))
    } else {
      rep(0, length(x))
    }
  } else {
    x
  }
}

#' Abort with the NONMEM error from .lst when a simulation produced no output
#'
#' @param regimen_label label of the regimen that failed
#' @param fit_folder NONMEM run folder for this regimen
#' @noRd
abort_on_failed_sim <- function(regimen_label, fit_folder) {
  lst_path <- file.path(fit_folder, "run.lst")
  stderr_path <- file.path(fit_folder, "stderr")
  lst <- if(file.exists(lst_path)) readLines(lst_path, warn = FALSE) else character(0)
  err <- if(file.exists(stderr_path)) readLines(stderr_path, warn = FALSE) else character(0)

  ## Try to find a known NONMEM error marker and grab nearby context.
  ## Otherwise fall back to the tail of the .lst file.
  markers <- c(
    "AN ERROR WAS FOUND IN THE CONTROL STATEMENTS",
    "MESSAGE ISSUED FROM NMTRAN",
    "PROGRAM TERMINATED BY OBJ",
    "PRED EXIT CODE",
    "ERROR IN",
    "NUMERICAL DIFFICULTIES"
  )
  marker_hit <- which(stringr::str_detect(lst, paste(markers, collapse = "|")))
  if(length(marker_hit) > 0) {
    first <- marker_hit[1]
    snippet <- lst[seq(first, min(length(lst), first + 25))]
  } else if(length(lst) > 0) {
    snippet <- utils::tail(lst, 30)
  } else {
    snippet <- character(0)
  }

  ## Escape braces so cli/glue doesn't interpret raw NONMEM output as
  ## interpolation expressions.
  esc <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

  msg <- c(
    "NONMEM simulation produced no output for regimen {.val {regimen_label}}.",
    i = "Run folder: {.path {fit_folder}}"
  )
  if(length(snippet) > 0) {
    msg <- c(msg, "NONMEM output (run.lst):", paste0("  ", esc(snippet)))
  } else if(length(err) > 0) {
    msg <- c(msg, "NONMEM stderr:", paste0("  ", esc(utils::tail(err, 20))))
  } else {
    msg <- c(msg, x = "No run.lst or stderr found in run folder.")
  }
  cli::cli_abort(msg, class = "pharmr_extra_sim_failed")
}

#' Create a dictionary from given specs and default dictionary as fallback
#'
parse_data_dictionary <- function(
  dictionary,
  default = list(
    ID = "ID",
    TIME = "TIME",
    DV = "DV",
    EVID = "EVID",
    AMT = "AMT",
    CMT = "CMT",
    MDV = "MDV"
  )
) {
  updated <- default
  updated[names(dictionary)] <- dictionary
  updated
}
