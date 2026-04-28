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
    uncertainty_method = "none",
    per_step_options = NULL,
    tool = "nonmem",
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
  current_est <- model$execution_steps$to_dataframe()
  n_existing <- nrow(current_est)
  n_new <- length(estimation_method)
  if(verbose) {
    steps_str <- paste(estimation_method, collapse = " -> ")
    cli::cli_alert_info("Setting estimation step(s): {steps_str}")
  }
  ## Set or add each step. Use set_estimation_step for existing indices to
  ## preserve any step-level options (e.g. tool_options), and
  ## add_estimation_step for any additional steps beyond what already exists.
  for(i in seq_along(estimation_method)) {
    step_opts <- if(!is.null(per_step_options) && i <= length(per_step_options)) {
      per_step_options[[i]]
    } else {
      list()
    }
    if(i <= n_existing) {
      existing_method <- toupper(as.character(current_est$method[i]))
      requested_method <- estimation_method[i]
      ## Case 1: method matches and no user overrides — leave $EST untouched.
      if(identical(existing_method, requested_method) && length(step_opts) == 0) {
        if(verbose) {
          cli::cli_alert_info("Step {i}: {requested_method} already set, no overrides provided; leaving $EST unchanged.")
        }
        next
      }

      cov_record <- if(tool == "nonmem" && has_covariance_record(model)) {
        get_covariance_record(model)
      } else {
        NULL
      }
      # Save $TABLE records: pharmpy's set_estimation_step() may corrupt them
      # by appending predictions/residuals to the last $TABLE in wrong position
      saved_tables <- if(tool == "nonmem") get_table_records(model) else NULL

      ## Build desired_opts
      ## - if method differs: defaults overridden by user step_opts
      ## - if method matches: user step_opts only (don't re-apply defaults)
      if(identical(existing_method, requested_method)) {
        desired_opts <- step_opts
      } else {
        desired_opts <- get_estimation_options(tool, tolower(requested_method), step_opts)
      }

      ## Existing tool_options dict for this step, preserved unless overridden
      existing_tool_opts <- current_est$tool_options[[i]]
      if(is.null(existing_tool_opts)) existing_tool_opts <- list()
      existing_tool_opts <- as.list(existing_tool_opts)

      ## Split desired_opts into structured kwargs vs tool_options.
      structured_kwargs <- list()
      tool_options_update <- list()
      for(key in names(desired_opts)) {
        up_key <- toupper(key)
        if(up_key %in% names(STRUCTURED_OPTION_MAP)) {
          map <- STRUCTURED_OPTION_MAP[[up_key]]
          val <- desired_opts[[key]]
          if(identical(map$type, "integer")) {
            val <- as.integer(val)
          } else if(identical(map$type, "logical")) {
            val <- as.logical(val)
          }
          structured_kwargs[[map$field]] <- val
        } else {
          tool_options_update[[up_key]] <- desired_opts[[key]]
        }
      }

      ## Final tool_options: start from existing, apply non-structured updates.
      final_tool_options <- existing_tool_opts
      for(k in names(tool_options_update)) {
        final_tool_options[[k]] <- tool_options_update[[k]]
      }
      ## Strip any structured-field key from final_tool_options to prevent
      ## the pharmpy writer from emitting the option twice (it emits
      ## structured fields AND all tool_options without deduplication).
      for(struct_opt in names(STRUCTURED_OPTION_MAP)) {
        final_tool_options[[struct_opt]] <- NULL
      }
      ## Coerce remaining values to character (matches get_estimation_options).
      for(k in names(final_tool_options)) {
        final_tool_options[[k]] <- as.character(final_tool_options[[k]])
      }

      call_args <- c(
        list(
          model = model,
          method = requested_method,
          idx = i - 1L
        ),
        structured_kwargs,
        list(tool_options = final_tool_options)
      )
      model <- do.call(pharmr::set_estimation_step, call_args)

      if(!is.null(cov_record)) { # the previous command may reset the COV record
        model <- update_covariance_record(model, cov_record)
      }
      if(!is.null(saved_tables)) {
        model <- restore_table_records(model, saved_tables)
      }
    } else {
      tool_options_i <- get_estimation_options(tool, tolower(estimation_method[i]), step_opts)
      model <- pharmr::add_estimation_step(
        model,
        method = estimation_method[i],
        interaction = TRUE,
        parameter_uncertainty_method = if(tolower(uncertainty_method) == "none") NULL else uncertainty_method,
        residuals = character(0),
        predictions = character(0),
        derivatives = character(0),
        tool_options = tool_options_i
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

## Mapping from NONMEM $EST option keys to pharmpy EstimationStep structured
## fields. Keys in this map are emitted by pharmpy as first-class attributes
## (separate from the tool_options dict); passing them via tool_options in
## addition would cause duplication in the rendered $EST record.
STRUCTURED_OPTION_MAP <- list(
  MAXEVAL    = list(field = "maximum_evaluations",    type = "integer"),
  NITER      = list(field = "niter",                  type = "integer"),
  ISAMPLE    = list(field = "isample",                type = "integer"),
  PRINT      = list(field = "keep_every_nth_iter",    type = "integer"),
  AUTO       = list(field = "auto",                   type = "integer"),
  ETASAMPLES = list(field = "individual_eta_samples", type = "logical")
)
