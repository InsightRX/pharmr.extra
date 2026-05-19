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
  mu_reference = "auto",
  verbose = TRUE
) {
  if(!requireNamespace("nlmixr2", quietly = TRUE)) {
    cli::cli_abort(
      c("Package {.pkg nlmixr2} is required to fit nlmixr-format models.",
        i = "Install with {.code install.packages(\"nlmixr2\")}.")
    )
  }

  time_start <- Sys.time()

  ## MU-referencing — critical for nlmixr2 SAEM stability. Pharmpy's
  ## mu_reference_model() rewrites `CL <- POP_CL * exp(ETA_CL)` as
  ## `mu_1 <- log(POP_CL); CL <- exp(ETA_CL + mu_1)`, putting the parameter
  ## walk on log scale so the M-step can't drift positive THETAs negative.
  ## Idempotent (the `!has_mu_reference` check guards re-application).
  est_for_mu <- estimation_method %||% tryCatch(
    model$execution_steps$to_dataframe()$method,
    error = function(e) NULL
  )
  is_saem <- !is.null(est_for_mu) && "saem" %in% tolower(est_for_mu)
  is_mu_ref <- isTRUE(pharmr::has_mu_reference(model))
  if((isTRUE(mu_reference) || (identical(mu_reference, "auto") && is_saem)) && !is_mu_ref) {
    if(verbose) cli::cli_alert_info("Applying mu-referencing to model.")
    model <- pharmr::mu_reference_model(model)
    ## The mu-ref rewrite changes model$code, so invalidate any cached
    ## SAEM-safe code (it was computed against the un-mu-referenced form).
    attr(model, "nlmixr_code") <- NULL
  } else if(isFALSE(mu_reference) && is_saem && !is_mu_ref) {
    cli::cli_warn(
      "nlmixr2 SAEM benefits significantly from mu-referencing — without it the M-step can drift positive THETAs through zero. Consider {.code mu_reference = \"auto\"}."
    )
  }

  ## Resolve dataset: prefer explicit `data`, then `attr(model, 'original_data')`,
  ## then `model$dataset`. nlmixr2 wants a data.frame in memory.
  fit_data <- resolve_nlmixr_data(model, data)

  ## When the caller supplied an explicit `data`, pharmpy's `model$dataset`
  ## still points at whatever was attached at model-build time. Stash the
  ## actual fit dataset on the model so resolve_nlmixr_data() and
  ## create_vpc_data_nlmixr() pick it up when the saved fit is re-used.
  if(!is.null(data)) attr(model, "original_data") <- fit_data

  ## Use the SAEM-safe code cached by create_model() when present; otherwise
  ## re-apply the residual-alias cleanup + residual-bound enforcement on
  ## $code. The cached attribute can be lost across subsequent pharmpy ops
  ## (e.g. update_parameters), and models built outside create_model() never
  ## had it. Without this fallback SAEM hits `endpoint 'Y' for saem cannot
  ## locate the residual error(s) correctly` (alias issue) or drifts the
  ## residual σ negative (unbounded-init issue).
  model_code <- attr(model, "nlmixr_code") %||%
    make_nlmixr_saem_safe(model$code)

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
  lsoda_log_path <- file.path(fit_folder, "run_lsoda.log")
  raw_fit <- run_nlmixr2_in_subprocess(
    fn = nlmixr_fn,
    data = fit_data,
    est = est,
    control = control,
    log_path = log_path,
    lsoda_log_path = lsoda_log_path,
    verbose = verbose
  )
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
    ## update_parameters() returns a fresh pharmpy object — re-cache the
    ## SAEM-safe code so any later run_nlme()/run_sim() on the final model
    ## doesn't fall back to the raw alias pattern.
    attr(final_model, "nlmixr_code") <- make_nlmixr_saem_safe(final_model$code)
    attr(fit, "final_model") <- final_model
    if(save_final) {
      writeLines(attr(final_model, "nlmixr_code"), file.path(fit_folder, "final.R"))
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

#' Fit an nlmixr2 model in a child R process and capture all output
#'
#' `nlmixr2`/`rxode2` ODE warnings (notably the `EE:[lsoda] / intdy --`
#' cascade) are emitted by C/Fortran code that writes directly to OS-level
#' stdout/stderr, bypassing R's `sink()`. Running in-process with
#' `sink(type = "message")` only catches R-level messages and warnings, so
#' those lsoda lines stream to the user's terminal but never reach the
#' run.log. We instead fit in a child R process via [callr::r()] with
#' `stderr = "2>&1"`, which redirects the child's OS-level FDs into the
#' parent so we can route each line ourselves.
#'
#' Output routing: each captured line is sorted in real time:
#'   * lsoda/intdy/`@(lsoda.c:` lines → `run_lsoda.log` (these can run into
#'     the thousands during SAEM exploration and would otherwise swamp the
#'     main log)
#'   * everything else (iteration trace, status messages, R warnings) →
#'     `run.log`
#'   * if `verbose = TRUE`, non-lsoda lines are also echoed to the parent
#'     console (preserves the split-to-console feel of the old sink path)
#'
#' @noRd
run_nlmixr2_in_subprocess <- function(fn, data, est, control, log_path,
                                      lsoda_log_path, verbose) {
  if(!requireNamespace("callr", quietly = TRUE)) {
    cli::cli_abort(
      c("Package {.pkg callr} is required to run nlmixr2 with full output capture.",
        i = "Install with {.code install.packages(\"callr\")}.")
    )
  }

  ## Open both log files for the lifetime of the fit. They are closed on
  ## exit even if the child errors.
  main_con  <- file(log_path,       open = "wt")
  lsoda_con <- file(lsoda_log_path, open = "wt")
  on.exit({ close(main_con); close(lsoda_con) }, add = TRUE)

  ## Pattern matching anything that should go to the lsoda log instead of
  ## the main log. Covers all three forms we've observed:
  ##   "unhandled error message: EE:[lsoda] ..."
  ##   "intdy -- t = ... illegal ..."
  ##   " @(lsoda.c:<lineno>"  (the trailing source-location line)
  lsoda_re <- "EE:\\[?lsoda|^\\s*intdy --|^\\s*@\\(lsoda\\.c"
  route_line <- function(line) {
    if(grepl(lsoda_re, line, perl = TRUE)) {
      writeLines(line, lsoda_con)
    } else {
      writeLines(line, main_con)
      if(isTRUE(verbose)) cat(line, "\n", sep = "")
    }
  }

  callr::r(
    func = function(fn, data, est, control) {
      ## Bring nlmixr2's DSL helpers (ini/model) into the child's search
      ## path so the parsed model function evaluates the same as in-process.
      suppressPackageStartupMessages(library(nlmixr2est))
      fit_args <- list(object = fn, data = data, est = est)
      if(!is.null(control)) fit_args$control <- control
      do.call(nlmixr2est::nlmixr2, fit_args)
    },
    args = list(fn = fn, data = data, est = est, control = control),
    stderr = "2>&1",
    callback = route_line,
    spinner = FALSE
  )
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

#' Rewrite pharmpy-emitted nlmixr code into a SAEM-safe form
#'
#' Composes the SAEM-safety transforms applied to pharmpy's nlmixr output:
#'   1. [inline_nlmixr_residual_aliases()] — collapse `add_error`/`prop_error`
#'      indirection so the residual terms in `~` reference `ini()` params
#'      directly.
#'   2. [enforce_residual_bounds()] — stamp `c(0, init, Inf)` on the
#'      residual-error params so SAEM can't drift them negative.
#'   3. [enforce_theta_bounds()] — stamp `c(0, init, Inf)` on unbounded
#'      positive-init THETAs so SAEM's M-step can't push them through zero.
#'   4. [apply_ipred_guard()] — wire pharmpy's `IPREDADJ` floor-guard into
#'      `Y` so proportional/combined residuals don't collapse at IPRED == 0.
#'
#' Each transform is a no-op when its expected pattern isn't present, so this
#' is safe to apply unconditionally on any pharmpy-emitted nlmixr code.
#'
#' @noRd
make_nlmixr_saem_safe <- function(code) {
  apply_ipred_guard(
    enforce_theta_bounds(
      enforce_residual_bounds(
        inline_nlmixr_residual_aliases(code)
      )
    )
  )
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

  ## nlmixr2's SAEM parser rejects expressions inside add()/prop() — each must
  ## be a bare `ini()` parameter. The `use_template = TRUE` path emits
  ## `add_error <- RUV_ADD*W` with `W <- 1` defined inside model({}), which
  ## stays a multiplication after the alias substitution. Simplify
  ## `<param>*<var>` (or `<var>*<param>`) when `<var>` is assigned a numeric
  ## constant in the same code (so RUV_ADD*W with W=1 collapses to RUV_ADD).
  const_assigns <- regmatches(
    lines,
    regexec("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*<-\\s*(-?[0-9.eE+-]+)\\s*$", lines)
  )
  const_map <- list()
  for(m in const_assigns) {
    if(length(m) == 3) {
      val <- suppressWarnings(as.numeric(m[[3]]))
      if(!is.na(val)) const_map[[m[[2]]]] <- val
    }
  }
  simplify_alias <- function(expr) {
    m <- regmatches(expr,
                    regexec("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\*\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*$",
                            expr))[[1]]
    if(length(m) != 3) return(expr)
    lhs_const <- const_map[[m[[2]]]]
    rhs_const <- const_map[[m[[3]]]]
    if(!is.null(lhs_const) && lhs_const == 1) return(m[[3]])
    if(!is.null(rhs_const) && rhs_const == 1) return(m[[2]])
    if(!is.null(lhs_const) && lhs_const == 0) return("0")
    if(!is.null(rhs_const) && rhs_const == 0) return("0")
    expr
  }
  add_val  <- simplify_alias(add_val)
  prop_val <- simplify_alias(prop_val)

  parts <- character(0)
  if(add_val  != "0") parts <- c(parts, paste0("add(",  add_val,  ")"))
  if(prop_val != "0") parts <- c(parts, paste0("prop(", prop_val, ")"))
  if(length(parts) == 0) return(code)  # both 0; leave model alone
  indent <- sub("\\S.*$", "", lines[formula_idx])
  lines[formula_idx] <- paste0(indent, "Y ~ ", paste(parts, collapse = " + "))
  lines <- lines[-c(add_idx, prop_idx)]
  paste(lines, collapse = "\n")
}

#' Stamp `c(0, init, Inf)` bounds on residual-error params in nlmixr ini()
#'
#' Pharmpy emits THETAs with explicit `c(lower, init, upper)` bounds but
#' typically leaves the residual-error params unbounded — e.g.
#' \preformatted{
#'   sigma1 <- 0.01     # additive
#'   sigma  <- 0.09     # proportional
#' }
#' nlmixr2's SAEM is gradient-free and treats unbounded params as truly
#' unbounded, so `sigma1` can drift to negative values during the E-step.
#' Once σ goes negative the likelihood inverts sign, ETAs explode, and the
#' ODE solver gets driven into unphysical states (the source of the
#' `EE:[lsoda] / intdy --` warning cascade). pharmpy's downstream
#' `update_parameters()` also refuses to ingest the resulting negative inits
#' because every THETA has lower bound 0.
#'
#' This function scans the `ini({...})` block for any unbounded
#' `name <- <numeric>` line whose `name` appears inside an `add(...)` or
#' `prop(...)` call in the model formula (i.e. it's a residual-error param)
#' and rewrites it as `name <- c(0, <numeric>, Inf)`.
#'
#' Run *after* [inline_nlmixr_residual_aliases()] so the residual params
#' show up as bare identifiers inside `add()`/`prop()` (not as `add_error`
#' aliases). Returns the input unchanged if no such params are found.
#'
#' @noRd
enforce_residual_bounds <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  ini_start <- grep("^\\s*ini\\(", lines)
  if(length(ini_start) == 0) return(code)
  ini_close <- grep("^\\s*\\}\\)\\s*$", lines)
  ini_close <- ini_close[ini_close > ini_start[1]]
  if(length(ini_close) == 0) return(code)
  ini_end <- ini_close[1]

  ## Collect identifiers used inside add(...) / prop(...) in the Y formula
  y_idx <- grep("^\\s*Y\\s*~", lines)
  if(length(y_idx) == 0) return(code)
  residual_refs <- character(0)
  call_re <- "(?:add|prop)\\(([^)]+)\\)"
  for(idx in y_idx) {
    calls <- regmatches(lines[idx], gregexpr(call_re, lines[idx], perl = TRUE))[[1]]
    for(call in calls) {
      inside <- sub("^[a-z]+\\(", "", call)
      inside <- sub("\\)$", "", inside)
      ## Split on operators / whitespace to grab identifier tokens
      tokens <- strsplit(inside, "[*+\\-/\\s]+", perl = TRUE)[[1]]
      idents <- tokens[grepl("^[A-Za-z_][A-Za-z0-9_]*$", tokens)]
      residual_refs <- c(residual_refs, idents)
    }
  }
  residual_refs <- unique(residual_refs)
  if(length(residual_refs) == 0) return(code)

  ## Wrap unbounded numeric assignments inside ini({...})
  bound_re <- "^(\\s*)([A-Za-z_][A-Za-z0-9_]*)\\s*<-\\s*(-?[0-9.eE+-]+)\\s*$"
  changed <- FALSE
  for(i in seq(ini_start[1], ini_end)) {
    m <- regmatches(lines[i], regexec(bound_re, lines[i]))[[1]]
    if(length(m) == 4 && m[[3]] %in% residual_refs) {
      ## guard: skip if init is already non-positive (would invert the bound)
      val <- suppressWarnings(as.numeric(m[[4]]))
      if(is.na(val) || val <= 0) next
      lines[i] <- paste0(m[[2]], m[[3]], " <- c(0, ", m[[4]], ", Inf)")
      changed <- TRUE
    }
  }
  if(!changed) return(code)
  paste(lines, collapse = "\n")
}

#' Stamp `c(0, init, Inf)` bounds on unbounded positive-init THETAs
#'
#' Pharmpy's NONMEM backend emits `$THETA (0, init)` for population
#' parameters, but its nlmixr2 backend emits the bare `POP_CL <- init` form —
#' nlmixr2 SAEM treats unbounded params as truly unbounded, so the M-step can
#' push `POP_CL` (or any other PK theta) through zero into negative values.
#' Once that happens the structural model produces non-physical predictions
#' (e.g. negative CL means concentrations grow without bound), lsoda fails to
#' integrate, and the fit diverges (admiral SAEM on this codebase: POP_CL
#' crosses zero at iteration ~25 and reaches -7.7M by iteration 100).
#'
#' Scan the `ini({...})` block for any `name <- <strictly-positive numeric>`
#' line whose `name` is NOT already handled as a residual param by
#' [enforce_residual_bounds()], and rewrite as `name <- c(0, init, Inf)`.
#' Skips lines that already have bounds (`c(...)`, `fixed(...)`) and skips
#' negative inits to preserve intentional sign conventions.
#'
#' @noRd
enforce_theta_bounds <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  ini_start <- grep("^\\s*ini\\(", lines)
  if(length(ini_start) == 0) return(code)
  ini_close <- grep("^\\s*\\}\\)\\s*$", lines)
  ini_close <- ini_close[ini_close > ini_start[1]]
  if(length(ini_close) == 0) return(code)
  ini_end <- ini_close[1]

  bound_re <- "^(\\s*)([A-Za-z_][A-Za-z0-9_]*)\\s*<-\\s*(-?[0-9.eE+-]+)\\s*$"
  changed <- FALSE
  for(i in seq(ini_start[1], ini_end)) {
    m <- regmatches(lines[i], regexec(bound_re, lines[i]))[[1]]
    if(length(m) != 4) next
    val <- suppressWarnings(as.numeric(m[[4]]))
    if(is.na(val) || val <= 0) next  # leave negative/zero inits alone
    lines[i] <- paste0(m[[2]], m[[3]], " <- c(0, ", m[[4]], ", Inf)")
    changed <- TRUE
  }
  if(!changed) return(code)
  paste(lines, collapse = "\n")
}

#' Wire pharmpy's `IPREDADJ` floor-guard into `Y`
#'
#' For proportional and combined residual error, pharmpy emits a floor-guard
#' block of the form
#' \preformatted{
#'   IPRED <- A_CENTRAL/VC
#'   if (0 == IPRED) {
#'       IPREDADJ <- 2.225e-16
#'   } else {
#'       IPREDADJ <- IPRED
#'   }
#'   Y <- IPRED        # <-- bug: should be IPREDADJ
#'   Y ~ prop(sigma)
#' }
#' followed by `Y <- IPRED` — so `IPREDADJ` is declared but never used. With
#' `Y ~ prop(sigma)` against `Y = IPRED`, the residual variance collapses to
#' 0 wherever IPRED is 0 (e.g. before absorption, or for extreme ETAs during
#' SAEM's E-step). The likelihood inverts, ETAs blow up, lsoda gets driven
#' into unphysical states, and the `EE:[lsoda] / intdy --` warning cascade
#' floods stderr — what 0.0.0.9078's subprocess capture now lands in run.log.
#'
#' Rewrite `Y <- IPRED` to `Y <- IPREDADJ` when the guard block is present
#' (detected by `IPREDADJ <- IPRED` in the else branch). No-op when no guard
#' block exists (additive RUV — pharmpy correctly omits the block there).
#'
#' @noRd
apply_ipred_guard <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  if(!any(grepl("^\\s*IPREDADJ\\s*<-\\s*IPRED\\s*$", lines))) return(code)
  y_re <- "^(\\s*)Y\\s*<-\\s*IPRED\\s*$"
  y_idx <- grep(y_re, lines)
  if(length(y_idx) == 0) return(code)
  for(i in y_idx) {
    indent <- sub("\\S.*$", "", lines[i])
    lines[i] <- paste0(indent, "Y <- IPREDADJ")
  }
  paste(lines, collapse = "\n")
}

#' Inject `scale_observations` into pharmpy-generated nlmixr code
#'
#' Pharmpy emits the observation prediction as
#' \preformatted{
#'   IPRED <- A_CENTRAL/<vol>     # in model({})
#' }
#' To apply a unit-scaling factor (e.g. `scale = 1000` when DV is in ng/mL
#' but dose × volume gives mg/L), rewrite this as
#' \preformatted{
#'   S<n> <- <vol>/<scale>
#'   IPRED <- A_CENTRAL/S<n>
#' }
#' mirroring the NONMEM `S<n> = V/scale` convention written by
#' [set_compartment_scale()]. The compartment number is inferred from the
#' presence of `A_DEPOT` (oral → S2; otherwise → S1).
#'
#' Returns the original code unchanged if the expected `IPRED <- A_*/<vol>`
#' pattern is not present (e.g. hand-written models, ltbs).
#'
#' @noRd
inject_nlmixr_scaling <- function(code, scale) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  ipred_re <- "^(\\s*)IPRED\\s*<-\\s*(A_[A-Za-z0-9_]+)\\s*/\\s*([A-Za-z][A-Za-z0-9_]*)\\s*$"
  ipred_idx <- grep(ipred_re, lines)
  if(length(ipred_idx) != 1) return(code)
  m <- regmatches(lines[ipred_idx], regexec(ipred_re, lines[ipred_idx]))[[1]]
  indent <- m[2]
  amount <- m[3]
  vol    <- m[4]
  cn <- if(any(grepl("A_DEPOT", lines, fixed = TRUE))) 2L else 1L
  sx <- paste0("S", cn)
  s_line <- paste0(indent, sx, " <- ", vol, "/", scale)
  new_ipred <- paste0(indent, "IPRED <- ", amount, "/", sx)
  c(
    lines[seq_len(ipred_idx - 1L)],
    s_line,
    new_ipred,
    if(ipred_idx < length(lines)) lines[seq.int(ipred_idx + 1L, length(lines))]
  ) |>
    paste(collapse = "\n")
}
