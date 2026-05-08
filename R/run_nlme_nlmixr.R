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

  ## Set the run name on the model (drives the function name in the generated
  ## code, which we extract below).
  model <- pharmr::set_name(model = model, new_name = id)

  ## Build a fresh run folder (mirrors NONMEM layout — dataset.csv +
  ## run.R holding the nlmixr2 function).
  fit_folder <- create_run_folder(id = id, path = path, force = force, verbose)
  model_file <- "run.R"
  output_file <- "run.log"
  model_path <- file.path(fit_folder, model_file)
  dataset_path <- file.path(fit_folder, "data.csv")
  utils::write.csv(fit_data, dataset_path, row.names = FALSE, quote = FALSE)
  writeLines(model$code, model_path)

  ## Pull the function definition out of the generated code and eval it.
  ## (The generated script ends with a `fit <- nlmixr2(name, dataset, ...)`
  ## call that references undefined symbols; we drop it.)
  nlmixr_fn <- extract_nlmixr_function(model$code)
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
  fit <- as_pharmpy_shaped_fit(raw_fit, model)

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
as_pharmpy_shaped_fit <- function(raw_fit, model) {
  pf <- raw_fit$parFixedDf
  ## Population fixed-effect names + estimates
  est_fixed <- stats::setNames(pf$Estimate, rownames(pf))
  se_fixed  <- stats::setNames(pf$SE, rownames(pf))

  ## Random-effect (omega) variances on the diagonal — pharmpy names these
  ## IIV_<param>, while nlmixr2 names the omega rows ETA_<param>. Strip the
  ## ETA_ prefix and re-prefix with IIV_ so update_parameters() can match.
  om <- raw_fit$omega
  iiv_estimates <- if(!is.null(om) && nrow(om) > 0) {
    iiv_names <- paste0("IIV_", sub("^ETA_", "", rownames(om)))
    stats::setNames(diag(as.matrix(om)), iiv_names)
  } else {
    numeric(0)
  }

  parameter_estimates <- c(est_fixed, iiv_estimates)

  ## Filter to names the pharmpy model recognises so set_initial_estimates()
  ## doesn't reject unknown keys (e.g. residual error parameters whose
  ## scale conventions differ between nlmixr2 and pharmpy).
  known <- tryCatch(model$parameters$names, error = function(e) NULL)
  if(!is.null(known)) {
    parameter_estimates <- parameter_estimates[names(parameter_estimates) %in% known]
  }

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

  list(
    ofv = unname(raw_fit$objf),
    ofv_iterations = numeric(0),
    function_evaluations = NA_integer_,
    parameter_estimates = parameter_estimates,
    standard_errors = standard_errors,
    relative_standard_errors = rse,
    correlation_matrix = tryCatch(raw_fit$cor, error = function(e) NULL),
    estimation_runtime = unname(raw_fit$time$optimize),
    runtime_total = sum(unname(unlist(raw_fit$time)), na.rm = TRUE),
    minimization_successful = !isTRUE(grepl("error|fail", raw_fit$message %||% "", ignore.case = TRUE)),
    covstep_successful = !is.null(raw_fit$cov) && !identical(raw_fit$covMethod, ""),
    termination_cause = raw_fit$message %||% NA_character_,
    warnings = character(0),
    significant_digits = NA_real_,
    raw_nlmixr_fit = raw_fit
  )
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
