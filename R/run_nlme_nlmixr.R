#' Run a pharmpy nlmixr-format model with nlmixr2
#'
#' Internal companion to [run_nlme()]; called when the input model is in
#' pharmpy's nlmixr backend. The function turns the pharmpy nlmixr model into
#' an `rxode2`/`nlmixr2` function, fits it with [nlmixr2::nlmixr2()], and
#' wraps the result in a list that mimics the shape of a pharmpy
#' `ModelfitResults` so downstream helpers (`attach_fit_info()`,
#' `compare_nlme_fit()`, etc.) work without engine-specific branches.
#'
#' Pharmpy can in principle drive nlmixr2 itself, but that path requires the
#' Python `pyreadr` module which is not part of the standard install; calling
#' nlmixr2 directly is also faster (no R→Python→R round-trip).
#'
#' @inheritParams run_nlme
#' @keywords internal
run_nlme_nlmixr <- function(
  model,
  data = NULL,
  id,
  path = getwd(),
  estimation_method = NULL,
  control = NULL,
  force = NULL,
  save_fit = TRUE,
  save_summary = TRUE,
  save_final = TRUE,
  clean = TRUE,
  verbose = TRUE
) {
  if(!requireNamespace("nlmixr2", quietly = TRUE)) {
    cli::cli_abort(
      c("Package {.pkg nlmixr2} is required to fit nlmixr-format models.",
        i = "Install with {.code install.packages(\"nlmixr2\")}.")
    )
  }

  time_start <- Sys.time()

  ## Resolve dataset: prefer explicit `data`, then `attr(model, 'original_data')`,
  ## then `model$dataset`. nlmixr2 wants a data.frame in memory.
  fit_data <- resolve_nlmixr_data(model, data)

  ## When the caller supplied an explicit `data`, pharmpy's `model$dataset`
  ## still points at whatever was attached at model-build time. Stash the
  ## actual fit dataset on the model so resolve_nlmixr_data() and
  ## create_vpc_data_nlmixr() pick it up when the saved fit is re-used.
  if(!is.null(data)) attr(model, "original_data") <- fit_data

  ## Use the SAEM-safe code cached by create_model(); fall back to the
  ## pharmpy-generated $code verbatim for models built outside create_model().
  ## Note: pharmpy's default residual-alias pattern is not SAEM-compatible,
  ## so SAEM fits on a BYO model will hit the upstream nlmixr2 error.
  model_code <- attr(model, "nlmixr_code") %||% model$code

  ## Build a fresh run folder (mirrors NONMEM layout — dataset.csv +
  ## run.R holding the nlmixr2 function).
  fit_folder <- create_run_folder(id = id, path = path, force = force, verbose)
  model_file <- "run.R"
  output_file <- "run.log"
  model_path <- file.path(fit_folder, model_file)
  dataset_path <- file.path(fit_folder, "data.csv")
  utils::write.csv(fit_data, dataset_path, row.names = FALSE, quote = FALSE)
  writeLines(model_code, model_path)

  ## Pull the function definition out of the generated code and eval it.
  ## (The generated script ends with a `fit <- nlmixr2(name, dataset, ...)`
  ## call that references undefined symbols; we drop it.)
  nlmixr_fn <- extract_nlmixr_function(model_code)
  if(is.null(nlmixr_fn)) {
    cli::cli_abort("Could not extract an nlmixr2 model function from the model code.")
  }

  est <- if(is.null(estimation_method)) "focei" else tolower(estimation_method[[1]])
  if(est %in% c("foce")) est <- "focei"

  ## Fit
  if(verbose) {
    cli::cli_process_start(paste0("Starting nlmixr2 run in ", fit_folder))
  }
  log_path <- file.path(fit_folder, output_file)
  fit_args <- list(object = nlmixr_fn, data = fit_data, est = est)
  if(!is.null(control)) fit_args$control <- control
  if(verbose) {
    raw_fit <- do.call(nlmixr2::nlmixr2, fit_args)
  } else {
    log_con <- file(log_path, open = "wt")
    sink(log_con, type = "output")
    sink(log_con, type = "message")
    on.exit({
      sink(type = "message")
      sink(type = "output")
      close(log_con)
    }, add = TRUE)
    raw_fit <- do.call(nlmixr2::nlmixr2, fit_args)
    sink(type = "message")
    sink(type = "output")
    close(log_con)
    on.exit()
  }
  if(verbose) cli::cli_process_done()

  ## Build a uniform fit object (pharmpy-shaped).
  fit <- as_pharmpy_shaped_fit(raw_fit, model, input_data = fit_data)

  ## Attach model + tables + info, same surface as the NONMEM path.
  fit <- attach_fit_info_nlmixr(
    fit = fit,
    model = model,
    raw_fit = raw_fit,
    fit_folder = fit_folder
  )

  ## Build & store a "final" pharmpy model with updated estimates so that
  ## run_sim() / create_vpc_data() can rely on `attr(fit, 'final_model')`.
  final_model <- update_parameters(model, fit)
  if(!is.null(final_model)) {
    if(!is.null(data)) attr(final_model, "original_data") <- fit_data
    attr(fit, "final_model") <- final_model
    if(save_final) {
      writeLines(final_model$code, file.path(fit_folder, "final.R"))
    }
  }

  ## Persist
  if(!is.null(save_fit)) {
    if(inherits(save_fit, "character")) {
      saveRDS(fit, save_fit)
    } else if(isTRUE(save_fit)) {
      saveRDS(fit, paste0(id, ".rds"))
    }
  }
  if(save_summary) {
    fit_summ <- create_modelfit_info_table(fit)
    txt_summ <- knitr::kable(fit_summ, row.names = FALSE, format = "simple")
    writeLines(txt_summ, paste0(id, "_fit_summary.txt"))
    par_est <- create_modelfit_parameter_table(fit)
    utils::write.csv(par_est, paste0(id, "_fit_parameters.csv"), quote = FALSE, row.names = FALSE)
  }

  if(verbose) {
    elapsed <- round(as.numeric(Sys.time() - time_start), 1)
    cli::cli_alert_success(paste0("Run done (", elapsed, "s)."))
  }

  fit
}

#' Resolve dataset for an nlmixr fit
#'
#' @noRd
resolve_nlmixr_data <- function(model, data) {
  if(!is.null(data)) {
    if(inherits(data, "character")) {
      return(utils::read.csv(data, check.names = FALSE))
    }
    if(inherits(data, "data.frame")) {
      return(as.data.frame(data))
    }
    cli::cli_abort("`data` must be a data.frame or path to a CSV file.")
  }
  original <- attr(model, "original_data")
  if(!is.null(original)) return(as.data.frame(original))
  if(!is.null(model$dataset)) return(as.data.frame(model$dataset))
  cli::cli_abort("No dataset available for nlmixr2 fit. Provide `data` or attach a dataset to the model.")
}

#' Extract the nlmixr2 model function from pharmpy-generated R code
#'
#' Pharmpy emits an R script of the form
#' \preformatted{
#' <name> <- function() { ini({...}) model({...}) }
#' fit <- nlmixr2(<name>, dataset, est = "focei", ...)
#' }
#' We need just the function — the trailing call references symbols that
#' don't exist at parse time (`dataset`).
#'
#' @noRd
extract_nlmixr_function <- function(code) {
  exprs <- tryCatch(parse(text = code), error = function(e) NULL)
  if(is.null(exprs)) return(NULL)
  env <- new.env()
  for(i in seq_along(exprs)) {
    e <- exprs[[i]]
    if(length(e) >= 3 &&
       (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
       is.call(e[[3]]) &&
       identical(e[[3]][[1]], as.name("function"))) {
      eval(e, envir = env)
      nm <- as.character(e[[2]])
      return(env[[nm]])
    }
  }
  NULL
}

#' Wrap a raw nlmixr2 fit into the same shape as a pharmpy ModelfitResults
#'
#' Exposes the keys used by [get_fit_info()], [create_modelfit_info_table()],
#' [create_modelfit_parameter_table()], and [update_parameters()].
#'
#' @noRd
as_pharmpy_shaped_fit <- function(raw_fit, model, input_data = NULL) {
  pf <- raw_fit$parFixedDf
  se_fixed  <- stats::setNames(pf$SE, rownames(pf))

  ## Use the shared extraction helper so block-omega off-diagonals
  ## (pharmpy parameter name IIV_X_IIV_Y) are also picked up. The helper
  ## handles ETA_X NaN-row filtering and pharmpy-name mapping for us.
  parameter_estimates <- nlmixr_parameter_estimates(raw_fit, model = model)
  if(is.null(parameter_estimates)) parameter_estimates <- numeric(0)

  ## Build standard_errors and relative_standard_errors aligned with
  ## parameter_estimates by name. nlmixr2 reports SEs only for fixed effects;
  ## fill NA where unavailable (matches pharmpy behaviour for IIV terms).
  standard_errors <- stats::setNames(rep(NA_real_, length(parameter_estimates)),
                                     names(parameter_estimates))
  match_idx <- match(names(standard_errors), names(se_fixed))
  has_se <- !is.na(match_idx)
  standard_errors[has_se] <- se_fixed[match_idx[has_se]]
  rse <- ifelse(parameter_estimates != 0,
                abs(standard_errors / parameter_estimates),
                NA_real_)
  rse <- stats::setNames(rse, names(parameter_estimates))

  ## Build pharmpy-style predictions / residuals frames from the nlmixr2 fit
  ## data.frame. pharmpy's ModelfitResults exposes these as top-level slots
  ## (e.g. `fit$predictions`), so example/GoF code that reads `fit$predictions`
  ## works regardless of engine.
  ##
  ## Match pharmpy's row-shape convention: `predictions` keeps all rows from
  ## the input dataset (NA for non-observation events), so that
  ## `bind_cols(model$dataset, fit$predictions)` works. `residuals` is
  ## observation-only (pharmpy filters non-obs out — see `_parse_residuals`).
  fit_df <- tryCatch(as.data.frame(raw_fit), error = function(e) NULL)
  pred_cols <- c("PRED", "IPRED", "CPRED", "CIPREDI", "EPRED")
  res_cols  <- c("RES", "IRES", "WRES", "IWRES", "CWRES", "CWRESI")
  predictions <- if(!is.null(fit_df)) {
    expand_predictions_to_full_dataset(
      fit_df[, intersect(pred_cols, names(fit_df)), drop = FALSE],
      input_data
    )
  } else NULL
  residuals   <- if(!is.null(fit_df)) fit_df[, intersect(res_cols,  names(fit_df)), drop = FALSE] else NULL

  out <- list(
    ofv = unname(raw_fit$objf),
    ofv_iterations = numeric(0),
    function_evaluations = NA_integer_,
    parameter_estimates = parameter_estimates,
    standard_errors = standard_errors,
    relative_standard_errors = rse,
    correlation_matrix = tryCatch(raw_fit$cor, error = function(e) NULL),
    predictions = predictions,
    residuals = residuals,
    estimation_runtime = unname(raw_fit$time$optimize),
    runtime_total = sum(unname(unlist(raw_fit$time)), na.rm = TRUE),
    minimization_successful = !isTRUE(grepl("error|fail", raw_fit$message %||% "", ignore.case = TRUE)),
    covstep_successful = !is.null(raw_fit$cov) && !identical(raw_fit$covMethod, ""),
    termination_cause = raw_fit$message %||% NA_character_,
    warnings = character(0),
    significant_digits = NA_real_,
    raw_nlmixr_fit = raw_fit
  )
  class(out) <- c("nlmixr2_modelfit_results", "list")
  out
}

#' Attach model, tables, and fit info to an nlmixr-shaped fit
#'
#' Mirrors [attach_fit_info()] for the NONMEM path. The default table is
#' named `sdtab` so example code that reads `attr(fit, "tables")$sdtab`
#' continues to work.
#'
#' @noRd
attach_fit_info_nlmixr <- function(fit, model, raw_fit, fit_folder) {
  attr(fit, "model") <- model

  ## Build sdtab-equivalent from the fit data.frame.
  fit_df <- as.data.frame(raw_fit)
  ## ID may come back as a factor — restore numeric to match NONMEM
  ## conventions used by example code.
  if("ID" %in% names(fit_df) && is.factor(fit_df$ID)) {
    fit_df$ID <- as.numeric(as.character(fit_df$ID))
  }
  attr(fit, "tables") <- list(sdtab = fit_df)

  attr(fit, "info") <- get_fit_info_nlmixr(fit)
  fit
}

#' Build the info attribute for an nlmixr-shaped fit
#'
#' @noRd
get_fit_info_nlmixr <- function(fit) {
  raw <- fit$raw_nlmixr_fit
  shrink <- if(!is.null(raw$shrink)) raw$shrink else NULL
  eta_shr <- if(!is.null(shrink) && "SD" %in% rownames(shrink)) {
    shrink["SD", , drop = TRUE]
  } else {
    numeric(0)
  }
  fit_info <- list(
    ofv = fit$ofv,
    condition_number = NA_real_,
    shrinkage = list(eta = eta_shr),
    eta_bar = "TODO",
    iterations = length(fit$ofv_iterations),
    function_evaluations = fit$function_evaluations,
    parameter_estimates = fit$parameter_estimates,
    standard_errors = fit$standard_errors,
    relative_standard_errors = fit$relative_standard_errors,
    runtime = list(
      estimation = fit$estimation_runtime,
      total = fit$runtime_total
    ),
    run_info = list(
      minimization_successful = ifelse(isTRUE(fit$minimization_successful), "yes", "no"),
      covstep_successful = ifelse(isTRUE(fit$covstep_successful), "yes", "no"),
      termination_cause = fit$termination_cause,
      warnings = as.character(fit$warnings),
      significant_digits = fit$significant_digits
    )
  )
  class(fit_info) <- c("list", "pharmpy_fit_info")
  fit_info
}

`%||%` <- function(x, y) if(is.null(x)) y else x

#' Expand an observation-level prediction frame to full input-dataset shape
#'
#' nlmixr2's `as.data.frame(fit)` returns one row per evaluated observation;
#' dose and other non-observation events are dropped. Pharmpy on the NONMEM
#' side keeps all rows (NaN at non-obs), so that
#' `bind_cols(model$dataset, fit$predictions)` works. This helper inserts
#' NA rows at the non-observation positions to match that convention.
#'
#' Falls back to the obs-only frame if observation rows can't be identified
#' (no EVID/MDV column) or the count doesn't match (e.g. nlmixr2 dropped
#' some rows during fitting — LLOQ handling, missing DV, etc.).
#'
#' @noRd
expand_predictions_to_full_dataset <- function(obs_df, input_data) {
  if(is.null(obs_df) || ncol(obs_df) == 0) return(obs_df)
  if(is.null(input_data) || nrow(input_data) == 0) return(obs_df)
  obs_idx <- find_observation_rows(input_data)
  if(is.null(obs_idx) || length(obs_idx) != nrow(obs_df)) return(obs_df)
  n_total <- nrow(input_data)
  out <- as.data.frame(
    lapply(obs_df, function(col) {
      res <- rep(col[NA_integer_], n_total)
      res[obs_idx] <- col
      res
    }),
    stringsAsFactors = FALSE
  )
  names(out) <- names(obs_df)
  out
}

#' Identify observation-row positions in a NONMEM-style dataset
#'
#' Prefers MDV (the canonical observation flag) and falls back to EVID == 0.
#' Returns NULL when neither column is present so callers can decide how to
#' degrade gracefully.
#'
#' @noRd
find_observation_rows <- function(data) {
  if("MDV" %in% names(data)) return(which(data$MDV == 0))
  if("EVID" %in% names(data)) return(which(data$EVID == 0))
  NULL
}

#' Inline pharmpy's residual-error aliases for SAEM compatibility
#'
#' Pharmpy's nlmixr converter always emits the residual block as
#' \preformatted{
#'   add_error <- <expr_a>     # in model({})
#'   prop_error <- <expr_p>
#'   Y ~ add(add_error) + prop(prop_error)
#' }
#' nlmixr2's SAEM rejects this — residual-error terms in the `~` formula must
#' reference `ini()` parameters directly, not variables computed in
#' `model({})`. Rewrite the formula to use `<expr_a>` / `<expr_p>` inline,
#' drop terms that resolve to 0, and remove the alias assignments. The
#' transformed form is equally valid for focei, so we apply it unconditionally.
#'
#' Returns the original code unchanged if the expected pattern is not present
#' (e.g. ltbs, hand-written models).
#'
#' @noRd
inline_nlmixr_residual_aliases <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  add_idx <- grep("^\\s*add_error\\s*<-\\s*", lines)
  prop_idx <- grep("^\\s*prop_error\\s*<-\\s*", lines)
  formula_idx <- grep(
    "^\\s*Y\\s*~\\s*add\\(\\s*add_error\\s*\\)\\s*\\+\\s*prop\\(\\s*prop_error\\s*\\)\\s*$",
    lines
  )
  if(length(add_idx) != 1 || length(prop_idx) != 1 || length(formula_idx) != 1) {
    return(code)
  }
  add_val  <- trimws(sub("^\\s*add_error\\s*<-\\s*",  "", lines[add_idx]))
  prop_val <- trimws(sub("^\\s*prop_error\\s*<-\\s*", "", lines[prop_idx]))
  parts <- character(0)
  if(add_val  != "0") parts <- c(parts, paste0("add(",  add_val,  ")"))
  if(prop_val != "0") parts <- c(parts, paste0("prop(", prop_val, ")"))
  if(length(parts) == 0) return(code)  # both 0; leave model alone
  indent <- sub("\\S.*$", "", lines[formula_idx])
  lines[formula_idx] <- paste0(indent, "Y ~ ", paste(parts, collapse = " + "))
  lines <- lines[-c(add_idx, prop_idx)]
  paste(lines, collapse = "\n")
}
