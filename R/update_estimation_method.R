#' Wrapper around pharmr's functions to set/add estimation methods
#'
#' The current pharmpy functionality is not stable, hence the need for this
#' wrapper.
#'
#' @inheritParams run_nlme
#'
update_estimation_method <- function(
    model,
    estimation_method,
    verbose = TRUE
) {
  if(length(estimation_method) < 1) {
    cli::cli_abort("At least one estimation method must be provided.")
  }
  estimation_method <- toupper(estimation_method)
  allowed <- c("FO", "FOCE", "ITS", "IMPMAP", "IMP", "SAEM") # FIXME: "LAPLACE" not allowed
  if (any(is.na(estimation_method)) ||
      any(! estimation_method %in% allowed, na.rm = TRUE)) {
    cli::cli_abort("The requested estimation method was not recognized. Available estimation methods are {allowed} or their lower-case equivalents.")
  }
  n_existing <- nrow(model$execution_steps$to_dataframe())
  n_new <- length(estimation_method)
  if(verbose) {
    steps_str <- paste(estimation_method, collapse = " -> ")
    cli::cli_alert_info("Setting estimation step(s): {steps_str}")
  }
  ## Set or add each step. Use set_estimation_step for existing indices to
  ## preserve any step-level options (e.g. tool_options), and
  ## add_estimation_step for any additional steps beyond what already exists.
  for(i in seq_along(estimation_method)) {
    if(i <= n_existing) {
      model <- pharmr::set_estimation_step(
        model,
        method = estimation_method[i],
        idx = i - 1L  # 0-indexed
      )
    } else {
      niter = NULL
      if(estimation_method[i] == "IMP") { ## TODO: Temporary fix
        niter <- 10
      }
      model <- pharmr::add_estimation_step(
        model,
        method = estimation_method[i],
        niter = niter,
        interaction = TRUE,
        residuals = character(0),
        predictions = character(0),
        derivatives = character(0),
        tool_options = list()
        # In pharmr>=2.0.0 the R wrapper defaults residuals/predictions/derivatives to c()
        # (NULL in R → None in Python) and tool_options to {} (also NULL → None).
        # None is not iterable, causing TypeError in pharmpy's update_estimation() and
        # frozenmapping() respectively. Passing explicit empty values avoids this.
      )
    }
  }
  ## Remove any leftover steps beyond the new set (reverse order to keep indices stable)
  if(n_existing > n_new) {
    for(idx in rev(seq_len(n_existing - n_new) + n_new - 1L)) {
      model <- pharmr::remove_estimation_step(model, idx)
    }
  }
  model
}
