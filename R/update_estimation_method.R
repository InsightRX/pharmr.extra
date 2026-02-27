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
  estimation_method <- toupper(estimation_method)
  allowed <- c("FO", "FOCE", "ITS", "IMPMAP", "IMP", "SAEM") # FIXME: "LAPLACE" not allowed
  if(any(! estimation_method %in% allowed)) {
    cli::cli_abort("The requested estimation method was not recognized. Available estimation methods are {allowed} or their lower-case equivalents.")
  }

  existing_steps <- model$execution_steps$to_dataframe()
  n_existing <- nrow(existing_steps)
  n_new <- length(estimation_method)

  ## Modify existing steps in place
  n_modify <- min(n_existing, n_new)
  for(i in seq_len(n_modify)) {
    model <- pharmr::set_estimation_step(
      model,
      method = estimation_method[i],
      idx = i - 1
    )
    if(verbose) {
      cli::cli_alert_info("Setting estimation step {i} to {estimation_method[i]}")
    }
  }

  ## Add new steps if more methods were requested than currently exist
  if(n_new > n_existing) {
    for(i in seq(n_existing + 1, n_new)) {
      ## Pass residuals = character(0) explicitly to avoid a TypeError in some
      ## Pharmpy versions where residuals defaults to None instead of ().
      model <- pharmr::add_estimation_step(
        model,
        method = estimation_method[i],
        residuals = character(0)
      )
      if(verbose) {
        cli::cli_alert_info("Adding estimation step {i}: {estimation_method[i]}")
      }
    }
  }

  ## Remove excess steps if fewer methods were requested (highest index first)
  if(n_new < n_existing) {
    for(i in seq(n_existing - 1, n_new, by = -1)) {
      model <- pharmr::remove_estimation_step(model, idx = i)
    }
  }

  model
}
