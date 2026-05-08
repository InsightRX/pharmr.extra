#' Run a simulation against an nlmixr-format model with rxode2
#'
#' Internal companion to [run_sim()]; called when the input model is a
#' pharmpy nlmixr-backend model. Uses [rxode2::rxSolve()] directly so we
#' can avoid the pharmpy-driven nlmixr fitting/simulation path (which
#' requires the Python `pyreadr` package).
#'
#' Returns a data.frame in the same shape as the NONMEM-side simulation
#' output (`ID`, `TIME`, `DV`, `IPRED`, `PRED`, `EVID`, plus declared
#' variables and a `regimen_label` column), so downstream example code
#' that plots simulation results works unchanged.
#'
#' Limitation: `PRED` is reported as `IPRED` (no separate population
#' prediction); rxSolve does not produce both in a single call.
#'
#' @inheritParams run_sim
#' @keywords internal
run_sim_nlmixr <- function(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = irxutils::get_random_id("sim_"),
  n_iterations = 1,
  variables = NULL,
  add_pk_variables = FALSE,
  output_file = "simtab",
  seed = 12345,
  verbose = TRUE
) {
  if(!requireNamespace("rxode2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg rxode2} is required for nlmixr2 simulations.")
  }

  ## Resolve model with current estimates
  if(is.null(model)) {
    if(is.null(fit)) {
      cli::cli_abort("Need either `fit` or `model` to simulate.")
    }
    model <- attr(fit, "final_model")
    if(is.null(model)) model <- attr(fit, "model")
    if(is.null(model)) {
      cli::cli_abort("Could not resolve a model from the supplied `fit`.")
    }
  }

  input_data <- as.data.frame(model$dataset)

  if(is.null(data)) {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
    sim_data[[".regimen"]] <- "original regimens"
  } else {
    if(!inherits(data, "data.frame")) {
      cli::cli_abort(
        c("`data` must be a data.frame (typically the output of {.fn create_sim_dataset}).",
          x = "Got an object of class {.cls {class(data)}}.")
      )
    }
    sim_data <- as.data.frame(data)
    if(!".regimen" %in% names(sim_data)) {
      sim_data[[".regimen"]] <- "original regimens"
    }
  }

  ## Extract the rxode2/nlmixr2 function from the pharmpy model code.
  nlmixr_fn <- extract_nlmixr_function(model$code)
  if(is.null(nlmixr_fn)) {
    cli::cli_abort("Could not extract an nlmixr2 model function from the model code.")
  }

  unique_regimens <- unique(sim_data[[".regimen"]])
  comb <- list()
  set.seed(seed)

  for(reg_label in unique_regimens) {
    if(verbose) cli::cli_alert_info("Running simulation ({reg_label})")
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

    raw_sim <- rxode2::rxSolve(
      object = nlmixr_fn,
      events = sim_data_regimen,
      nsim = n_iterations,
      returnType = "data.frame"
    )

    ## Reshape rxSolve output to the NONMEM-style sdtab columns expected
    ## by example code.
    out_df <- shape_rxsolve_output(raw_sim, sim_data_regimen)

    if(!is.null(variables)) {
      keep <- intersect(variables, names(out_df))
      always <- intersect(c("ID", "TIME", "DV", "IPRED", "PRED", "EVID", "sim.id"),
                          names(out_df))
      out_df <- out_df[, unique(c(always, keep)), drop = FALSE]
    }
    if(add_pk_variables) {
      regimen_for_pk <- NULL
      if("EVID" %in% names(sim_data_regimen) && "AMT" %in% names(sim_data_regimen)) {
        dose_rows <- sim_data_regimen[sim_data_regimen$EVID == 1, , drop = FALSE]
        if(nrow(dose_rows) > 0) {
          regimen_for_pk <- list(dose = dose_rows$AMT)
        }
      }
      out_df <- calc_pk_variables(data = out_df, regimen = regimen_for_pk)
    }
    out_df$regimen_label <- reg_label
    comb[[reg_label]] <- out_df
  }

  out <- dplyr::bind_rows(comb)
  if(verbose) cli::cli_alert_success("Done")
  out
}

#' Shape rxSolve output to match the NONMEM simulation table convention
#'
#' rxSolve emits `id`, `time`, `ipredSim`, `sim`, plus all declared
#' variables. The NONMEM-side simulation produces `ID`, `TIME`, `IPRED`,
#' `DV`, `PRED`, `EVID`, etc. We rename the rxSolve columns and merge in
#' `EVID`/`AMT`/`MDV` from the input dataset so that downstream filters
#' like `filter(EVID == 0)` keep working.
#'
#' @noRd
shape_rxsolve_output <- function(raw_sim, sim_data_regimen) {
  df <- as.data.frame(raw_sim)
  ## Pharmpy's nlmixr code typically declares `IPRED` directly in the
  ## model({}) block, so it appears in rxSolve output already. Drop
  ## rxSolve's auto-emitted `ipredSim` to avoid a duplicate name.
  if("IPRED" %in% names(df) && "ipredSim" %in% names(df)) {
    df[["ipredSim"]] <- NULL
  }
  rename_pairs <- c(id = "ID", time = "TIME", ipredSim = "IPRED", sim = "DV")
  for(old_nm in names(rename_pairs)) {
    new_nm <- rename_pairs[[old_nm]]
    if(old_nm %in% names(df) && !new_nm %in% names(df)) {
      names(df)[names(df) == old_nm] <- new_nm
    } else if(old_nm %in% names(df) && new_nm %in% names(df)) {
      ## both present → drop the rxSolve auto-name in favour of the
      ## model-declared one
      df[[old_nm]] <- NULL
    }
  }
  ## PRED is not directly available from rxSolve; report IPRED as a stand-in
  ## (acceptable for plotting; document the approximation upstream).
  if(!"PRED" %in% names(df) && "IPRED" %in% names(df)) {
    df$PRED <- df$IPRED
  }
  ## Coerce ID to numeric (rxSolve often returns it as factor/character).
  if("ID" %in% names(df) && !is.numeric(df$ID)) {
    df$ID <- suppressWarnings(as.numeric(as.character(df$ID)))
  }
  ## Carry EVID / MDV / AMT from input by joining on ID + TIME. rxSolve
  ## returns observation timepoints only, so restrict the join source to
  ## observation rows (and to the first hit per ID+TIME) to avoid
  ## row-duplication if doses share a timepoint with obs.
  carry <- intersect(c("EVID", "MDV", "AMT", "RATE", "CMT"), names(sim_data_regimen))
  if(length(carry) > 0 && all(c("ID", "TIME") %in% names(df))) {
    src <- sim_data_regimen
    if("EVID" %in% names(src)) src <- src[src$EVID == 0, , drop = FALSE]
    src <- src[!duplicated(src[, c("ID", "TIME"), drop = FALSE]), , drop = FALSE]
    df <- df |>
      dplyr::left_join(
        src[, unique(c("ID", "TIME", carry)), drop = FALSE],
        by = c("ID", "TIME")
      )
    if("EVID" %in% names(df)) df$EVID[is.na(df$EVID)] <- 0L
    if("MDV" %in% names(df)) df$MDV[is.na(df$MDV)] <- 0L
  }
  df
}
