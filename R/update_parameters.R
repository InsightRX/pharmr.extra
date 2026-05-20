#' Update parameter estimates (and fix)
#'
#' For example for using model in simulations.
#'
#' Supports both NONMEM-backend and nlmixr-backend pharmpy models. The `fit`
#' argument may be:
#' - a pharmpy `ModelfitResults` object (NONMEM path)
#' - the pharmpy-shaped wrapper returned by [run_nlme()] on an nlmixr model
#' - a raw `nlmixr2FitCore` object (output of [nlmixr2::nlmixr2()] called
#'   directly, without going through [run_nlme()])
#'
#' For nlmixr2 fits, both diagonal and off-diagonal omega elements are
#' extracted so that block-omega parameters (named `IIV_X_IIV_Y` in pharmpy)
#' are updated alongside the variance terms.
#'
#' @inheritParams attach_fit_info
#' @param fix fix the estimates?
#'
#' @returns the input pharmpy model with parameter estimates updated, or
#'   `invisible(NULL)` if no estimates were available
#'
#' @export
update_parameters <- function(
    model,
    fit,
    fix = FALSE,
    verbose = FALSE
) {
  params <- extract_parameter_estimates(fit, model)
  if(is.null(params)) {
    cli::cli_warn("No parameter estimates found in fit object; cannot update model.")
    return(invisible())
  }
  if(all(is.nan(params))) {
    cli::cli_warn("No parameter estimates were available, not updating model.")
    return(invisible())
  }
  if(any(is.nan(params))) {
    params <- params[!is.nan(params)]
    cli::cli_alert_info("Only some parameters were estimated, updating only for {names(params)}.")
  }
  if(fix) {
    model <- pharmr::fix_parameters_to(
      model,
      params
    )
  } else {
    model <- pharmr::set_initial_estimates(
      model = model,
      inits = params
    )
  }
  model
}

#' Pull a named numeric vector of parameter estimates from a fit object
#'
#' Dispatches on the fit's class: pharmpy fits already expose
#' `parameter_estimates`, raw nlmixr2 fits do not and need their fixed-effect
#' and omega slots stitched together. Returns NULL when nothing usable can be
#' read out (so [update_parameters()] can warn and return).
#'
#' @noRd
extract_parameter_estimates <- function(fit, model = NULL) {
  if(is_raw_nlmixr2_fit(fit)) {
    return(nlmixr_parameter_estimates(fit, model = model))
  }
  ## pharmpy ModelfitResults and the pharmpy-shaped wrapper both expose this
  fit$parameter_estimates
}

#' Detect a raw `nlmixr2::nlmixr2()` fit object
#'
#' These inherit from `nlmixr2FitCore` / `nlmixr2FitData`. Distinct from the
#' pharmpy-shaped wrapper, which has class `nlmixr2_modelfit_results`.
#'
#' @noRd
is_raw_nlmixr2_fit <- function(fit) {
  inherits(fit, c("nlmixr2FitCore", "nlmixr2FitData"))
}

#' Convert a raw nlmixr2 fit into a named vector of pharmpy-style parameter estimates
#'
#' Mirrors pharmpy's `IIV_X` / `IIV_X_IIV_Y` naming so that
#' `pharmr::set_initial_estimates()` and `pharmr::fix_parameters_to()` can be
#' applied directly. When `model` is supplied, the result is filtered to
#' parameters the model recognises, and off-diagonals are emitted in whichever
#' of `IIV_X_IIV_Y` / `IIV_Y_IIV_X` the model uses.
#'
#' @noRd
nlmixr_parameter_estimates <- function(raw_fit, model = NULL) {
  pf <- raw_fit$parFixedDf
  est_fixed <- if(!is.null(pf) && nrow(pf) > 0) {
    v <- stats::setNames(pf$Estimate, rownames(pf))
    v[!is.na(v)]  # parFixedDf has NaN rows for ETA_X; drop them
  } else {
    numeric(0)
  }

  ## Back-transform log-in-ini parameters (lpop_X → POP_X = exp(lpop_X))
  ## when run_nlme_nlmixr() rewrote the model into the nlmixr2-idiomatic
  ## form for SAEM. See apply_nlmixr2_log_param().
  log_param_map <- attr(model, "log_param_map") %||% character(0)
  if(length(log_param_map) > 0 && length(est_fixed) > 0) {
    for(lpop_name in names(log_param_map)) {
      if(lpop_name %in% names(est_fixed)) {
        pop_name <- unname(log_param_map[[lpop_name]])
        names(est_fixed)[names(est_fixed) == lpop_name] <- pop_name
        est_fixed[pop_name] <- exp(est_fixed[pop_name])
      }
    }
  }

  om <- raw_fit$omega
  iiv_estimates <- numeric(0)
  if(!is.null(om) && nrow(om) > 0) {
    om <- as.matrix(om)
    diag_labels <- sub("^ETA_", "", rownames(om))
    diag_estimates <- stats::setNames(diag(om), paste0("IIV_", diag_labels))

    offdiag_estimates <- numeric(0)
    n <- nrow(om)
    if(n > 1) {
      known <- tryCatch(model$parameters$names, error = function(e) NULL)
      for(i in seq_len(n - 1)) {
        for(j in seq.int(i + 1, n)) {
          v <- om[i, j]
          if(is.na(v)) next
          nm <- paste0("IIV_", diag_labels[i], "_IIV_", diag_labels[j])
          alt <- paste0("IIV_", diag_labels[j], "_IIV_", diag_labels[i])
          ## When the model uses a different ordering for the block parameter
          ## name, prefer that. When the model has no block parameter at all,
          ## the entry will be filtered out below — there's no harm emitting it.
          if(!is.null(known) && !(nm %in% known) && (alt %in% known)) nm <- alt
          offdiag_estimates[nm] <- v
        }
      }
    }
    iiv_estimates <- c(diag_estimates, offdiag_estimates)
  }

  parameter_estimates <- c(est_fixed, iiv_estimates)

  ## Filter to names the pharmpy model recognises so set_initial_estimates()
  ## doesn't reject unknown keys (e.g. residual-error parameters whose scale
  ## conventions differ between nlmixr2 and pharmpy).
  known <- tryCatch(model$parameters$names, error = function(e) NULL)
  if(!is.null(known)) {
    parameter_estimates <- parameter_estimates[names(parameter_estimates) %in% known]
  }

  if(length(parameter_estimates) == 0) return(NULL)
  parameter_estimates
}
