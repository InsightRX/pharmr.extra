#' Run a simulation based on supplied parameters estimates,
#' and combine into proper format for VPC
#'
#' @param fit fit object from `pharmr::run_modelfit()`. Optional, can supply a
#' `model` and `parameters` argument
#' @param model pharmpy model object. Optional, can also only supply just a
#' `fit` object
#' @param parameters list of parameter estimates, e.g. `list(CL = 5, V = 50)`.
#' Optional, can also supply a `fit` object.
#' @param n number of simulation iterations to generate
#' @param keep_columns character vector of column names in original dataset
#' to keep in the output dataset
#' @param verbose verbose output?
#' @param id TODO
#' @param use_pharmpy TODO
#'
#' @returns TODO
#' 
#' @export
create_vpc_data <- function(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE,
  id = NULL,
  use_pharmpy = TRUE
) {

  ## Resolve model first (was being inspected before assignment).
  ## When only `fit` is supplied, prefer `final_model` (carries fitted
  ## estimates) over `model` (pre-fit, initial estimates) so VPCs don't
  ## silently simulate against the starting point.
  caller_supplied_model <- !is.null(model)
  if(is.null(model)) {
    model <- attr(fit, "final_model") %||% attr(fit, "model")
    if(is.null(model)) {
      cli::cli_abort("Either a `fit` object with a model attached, or a `model` argument is required.")
    }
  }
  tool <- get_tool_from_model(model)
  data <- model$dataset

  if(tool == "nlmixr") {
    return(create_vpc_data_nlmixr(
      fit = fit,
      model = if(caller_supplied_model) model else NULL,
      parameters = parameters,
      keep_columns = keep_columns,
      n = n,
      verbose = verbose
    ))
  }
  if(!is.null(parameters)) {
    if(verbose) message("Using supplied `parameters` object")
  } else { # try to grab from fit object
    if(!is.null(fit) && !is.null(fit$parameter_estimates)) {
      if(verbose) message("Using parameters from `fit` object")
      parameters <-  as.list(fit$parameter_estimates)
    } else {
      warning("No parameter estimates available, will use initial estimates for VPC!")
    }
  }

  if(is.null(model)) {
    if(verbose) message("Using model from fit object")
    model <- attr(fit, "model")
    if(is.null(model) || !inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_abort("Model is not a pharmpy Model object.")
    }
  }
  if(verbose) message("Updating estimates for simulation model")
  sim_model <- pharmr::set_initial_estimates(
    model,
    inits = parameters
  )

  ## Remove tables and covariance step, add back table with stuff that the VPC needs (ID TIME DV EVID MDV)
  keep <- unique(c("ENC_TIME", keep_columns))
  keep <- keep[keep %in% names(data)]
  sim_model <- sim_model |>
    pharmr::remove_parameter_uncertainty_step() |>
    remove_tables_from_model() |>
    add_table_to_model(
      variables = c("ID", "TIME", "PRED", "DV", "EVID", "MDV", keep),
      firstonly = FALSE,
      file = "sdtab"
    )

  ## Make sure data is clean for modelfit
  sim_model <- clean_modelfit_data(sim_model)

  tmp_path <- file.path(
    tempdir(),
    paste0("simulation_", irxutils::random_string(5))
  )
  dir.create(tmp_path)
  if(is.null(id)) {
    id <- "tmp"
  }

  ## Run maxeval=0 run to get obs dataset
  if(verbose) cli::cli_alert_info("Running input model evaluation for VPC")
  eval_model <- sim_model |>
    pharmr::set_evaluation_step(idx = 0)
  eval_res <- run_nlme(
    model = eval_model,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE
  )
  obs <- attr(eval_res, "tables")[[1]]

  ## Run the simulation
  if(verbose) cli::cli_alert_info("Running simulation for VPC")
  sim_model <- pharmr::set_simulation(
    sim_model,
    n = n
  )

  sim_data <- run_nlme(
    model = sim_model,
    path = tmp_path,
    force = TRUE,
    id = id,
    save_final = FALSE
  )
  sim <- attr(sim_data, "tables")[[1]]

  ## Parse the output and make ready for vpc::vpc()
  if(verbose) cli::cli_alert_info("Preparing simulated output data for plotting")

  ## Generate a TAD colunmn
  if(is.null(obs$TAD)) {
    obs <- obs |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(last_dose_time = dplyr::if_else(.data$EVID == 1, .data$TIME, NA)) |>
      tidyr::fill("last_dose_time", .direction = "downup") |>
      dplyr::mutate(TAD = .data$TIME - .data$last_dose_time) |>
      dplyr::select(-"last_dose_time")
  }

  ## Check if obs and sim match up, and make sure sim has the columns it needs
  len_obs <- nrow(obs)
  len_sim <- nrow(sim)
  if((len_sim %% len_obs) != 0) {
    cli::cli_abort("The simulated dataset length is not a multiple of the length of the original dataset. Please check model and simulation settings.")
  }
  if(use_pharmpy) {
    transfer <- c("ID", "TIME", "PRED", "TAD", "ENC_TIME")
    for(col in transfer) {
      if(!is.null(obs[[col]])) {
        sim[[col]] <- obs[[col]]
      } else {
        cli::cli_alert_warning("Column {col} not found in original dataset.")
      }
    }
  }
  for(col in keep_columns) {
    sim[[col]] <- obs[[col]]
  }

  ## Return
  list(obs = obs, sim = sim)
}

#' Build VPC obs/sim data for an nlmixr-format model
#'
#' Mirrors [create_vpc_data()] but stays in nlmixr2-land: the observation
#' dataset is taken from the model's data, and `n` simulation iterations
#' are produced via [run_sim()] (which dispatches to rxode2's `rxSolve`
#' for nlmixr2 models).
#'
#' @noRd
create_vpc_data_nlmixr <- function(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE
) {
  if(is.null(model)) {
    if(is.null(fit)) cli::cli_abort("Need either `fit` or `model`.")
    model <- attr(fit, "final_model")
    if(is.null(model)) model <- attr(fit, "model")
  }

  ## Update estimates if supplied (or, when a fit is given, take them from
  ## the fit). For nlmixr2 simulations the model already carries the final
  ## estimates if `attr(fit, 'final_model')` is used.
  if(!is.null(parameters)) {
    model <- pharmr::set_initial_estimates(model, inits = parameters)
  } else if(!is.null(fit) && !is.null(fit$parameter_estimates) &&
            is.null(attr(fit, "final_model"))) {
    model <- pharmr::set_initial_estimates(model, inits = as.list(fit$parameter_estimates))
  }

  ## Prefer an explicitly-attached input dataset (set by run_nlme_nlmixr
  ## when `data` was supplied) over `model$dataset`, which may not have
  ## been refreshed.
  data <- as.data.frame(attr(model, "original_data") %||% model$dataset)

  ## Build obs from the model dataset. Compute TAD on the full event log
  ## (dose rows are needed to derive last_dose_time), then restrict to
  ## observation rows so the row-set matches what rxSolve returns and
  ## downstream VPC alignment between obs and sim is preserved.
  obs <- data
  if(!"EVID" %in% names(obs)) obs$EVID <- 0L
  if(!"MDV" %in% names(obs)) obs$MDV <- ifelse(obs$EVID == 0, 0L, 1L)
  if(is.null(obs$TAD)) {
    obs <- obs |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(last_dose_time = dplyr::if_else(.data$EVID == 1, .data$TIME, NA_real_)) |>
      tidyr::fill("last_dose_time", .direction = "downup") |>
      dplyr::mutate(TAD = .data$TIME - .data$last_dose_time) |>
      dplyr::select(-"last_dose_time") |>
      dplyr::ungroup() |>
      as.data.frame()
  }
  obs <- obs[obs$MDV == 0, , drop = FALSE]

  ## Run n simulations against the same dataset; reuse run_sim() so the
  ## engine dispatch lives in one place.
  if(verbose) cli::cli_alert_info("Running {n} simulations for VPC")
  sim <- run_sim_nlmixr(
    fit = fit,
    data = NULL,        # use model dataset
    model = model,
    n_iterations = n,
    verbose = FALSE
  )

  ## Optional column carry-over from obs to sim
  for(col in keep_columns) {
    if(col %in% names(obs)) {
      ## sim has multiple iterations per obs row; left_join on (ID, TIME)
      sim <- sim |>
        dplyr::left_join(
          obs[, c("ID", "TIME", col), drop = FALSE] |> unique(),
          by = c("ID", "TIME"),
          suffix = c("", ".obs")
        )
      ## prefer existing column on sim; otherwise rename .obs back
      sim_col <- paste0(col, ".obs")
      if(sim_col %in% names(sim) && !col %in% names(sim)) {
        names(sim)[names(sim) == sim_col] <- col
      }
    }
  }

  list(obs = obs, sim = sim)
}
