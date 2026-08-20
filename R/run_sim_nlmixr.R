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
#' `PRED` is the population prediction, obtained from a second solve with the
#' between-subject random effects zeroed ([rxode2::zeroRe()]) — rxSolve cannot
#' emit `IPRED` and `PRED` in one call. If that solve is unavailable (older
#' rxode2 without `zeroRe()`) or fails, `PRED` falls back to `IPRED`.
#'
#' @inheritParams run_sim
#' @param path ignored for the nlmixr2 backend: simulations run via
#' [rxode2::rxSolve()] and create no NONMEM-style run folders. Accepted only to
#' keep the signature aligned with [run_sim()].
#' @param model_code pre-rendered nlmixr2/rxode2 model code (character), used
#' instead of extracting it from `model`. This is what lets a simulation run in
#' a worker process: a Pharmpy `model` is a Python (reticulate) object and
#' cannot cross a process boundary, but its rendered code can. When supplied,
#' `model`/`fit` are not needed and `data` is required.
#' @keywords internal
run_sim_nlmixr <- function(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = irxutils::get_random_id("sim_"),
  path = NULL,
  n_iterations = 1,
  variables = NULL,
  add_pk_variables = FALSE,
  output_file = "simtab",
  seed = 12345,
  verbose = TRUE,
  model_code = NULL
) {
  if(!requireNamespace("rxode2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg rxode2} is required for nlmixr2 simulations.")
  }

  ## Resolve model with current estimates. Skipped entirely when the caller
  ## already rendered the model code (parallel uncertainty replicates), since
  ## touching `model` would mean touching Python.
  if(is.null(model_code)) {
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
    ## Only when we are going to use it: `resolve_nlmixr_data()` aborts on a
    ## model with no dataset of any kind, which would turn a perfectly well
    ## specified `run_sim(model = , data = )` into an error, and it converts the
    ## whole dataset out of Python for a value the `data` branch below discards.
    ##
    ## Not `model$dataset`: when the caller passed an explicit `data =` to
    ## run_nlme(), the fitted dataset lives on `attr(model, "original_data")`
    ## and `model$dataset` still points at whatever was attached at model-build
    ## time. resolve_nlmixr_data() prefers the former.
    input_data <- if(is.null(data)) resolve_nlmixr_data(model, NULL) else NULL
  } else {
    if(is.null(data)) {
      cli::cli_abort("`data` is required when `model_code` is supplied.")
    }
    input_data <- NULL
  }

  if(is.null(data)) {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
    sim_data[[".regimen"]] <- "original regimens"
  } else {
    validate_sim_data(data)
    sim_data <- as.data.frame(data)
    if(!".regimen" %in% names(sim_data)) {
      sim_data[[".regimen"]] <- "original regimens"
    }
  }

  ## Extract the rxode2/nlmixr2 function from the pharmpy model code. Prefer
  ## the cached, post-processed code (set by create_model() — accounts for
  ## SAEM-safe residual aliases and scale_observations); otherwise re-apply
  ## the residual-alias cleanup, since the cached attribute can be lost across
  ## subsequent pharmpy ops (e.g. update_parameters returns a fresh object).
  model_code <- model_code %||%
    attr(model, "nlmixr_code") %||%
    make_nlmixr_saem_safe(model$code)
  nlmixr_fn <- extract_nlmixr_function(model_code)
  if(is.null(nlmixr_fn)) {
    cli::cli_abort("Could not extract an nlmixr2 model function from the model code.")
  }

  unique_regimens <- unique(sim_data[[".regimen"]])
  comb <- list()
  set.seed(seed)
  ## `set.seed()` on its own does not pin rxode2's RNG once solving is threaded;
  ## `rxSetSeed()` is the documented hook for that. Setting both is what makes a
  ## replicate reproduce identically whether it ran in this process or in a
  ## worker (which starts from a fresh rxode2 RNG state). Restored on exit,
  ## since rxSetSeed() is global and would otherwise pin the caller's session.
  if(rx_seed_supported()) {
    old_rx_seed <- rxode2::rxGetSeed()
    rxode2::rxSetSeed(seed)
    on.exit(try(rxode2::rxSetSeed(old_rx_seed), silent = TRUE), add = TRUE)
  }

  for(reg_label in unique_regimens) {
    if(verbose) cli::cli_alert_info("Running simulation ({reg_label})")
    sim_data_regimen <- sim_data |>
      dplyr::filter(.data$.regimen == reg_label) |>
      dplyr::select(-".regimen") |>
      sort_nlmixr_events()

    raw_sim <- rxode2::rxSolve(
      object = nlmixr_fn,
      events = sim_data_regimen,
      nsim = n_iterations,
      returnType = "data.frame"
    )

    ## Population prediction: the same events solved with the between-subject
    ## random effects zeroed, which is what makes prediction-corrected VPCs
    ## meaningful. Costs one extra single-replicate solve per regimen *per call*
    ## — so `run_sim(n_uncertainty = )` pays it once per replicate as well.
    ## Skipped where the model declares `PRED` itself, since shape_rxsolve_output()
    ## keeps the model's own column and would throw this result away.
    pop_pred <- if("PRED" %in% names(raw_sim)) {
      NULL
    } else {
      solve_population_prediction(nlmixr_fn, sim_data_regimen)
    }

    ## Reshape rxSolve output to the NONMEM-style sdtab columns expected
    ## by example code.
    out_df <- shape_rxsolve_output(raw_sim, sim_data_regimen, pop_pred = pop_pred)

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
#' @param pop_pred optional numeric vector of population predictions for one
#' simulation replicate (see [solve_population_prediction()]). `raw_sim` stacks
#' `nsim` contiguous replicates in identical row order, so the vector is
#' recycled across them. `NULL` (or a length that isn't exactly one replicate's
#' worth of rows) falls back to reporting `IPRED` as `PRED`.
#'
#' @noRd
shape_rxsolve_output <- function(raw_sim, sim_data_regimen, pop_pred = NULL) {
  df <- as.data.frame(raw_sim)
  ## How many replicates are stacked in `df`, taken from rxSolve's own marker
  ## (absent for a single-replicate solve). Used to check `pop_pred` covers
  ## exactly one replicate.
  nsim <- if("sim.id" %in% names(df)) length(unique(df[["sim.id"]])) else 1L
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
  ## PRED comes from a separate zero-random-effects solve (rxSolve cannot emit
  ## both in one call). Where that isn't available, report IPRED as a stand-in.
  ## The vector has to be exactly one replicate's worth of rows: mere
  ## divisibility would silently recycle a half-length (a dropped subject, a
  ## different row set) against the wrong rows instead of falling back.
  if(!"PRED" %in% names(df)) {
    if(!is.null(pop_pred) && length(pop_pred) > 0 &&
       length(pop_pred) * nsim == nrow(df)) {
      df$PRED <- rep(pop_pred, times = nsim)
    } else if("IPRED" %in% names(df)) {
      df$PRED <- df$IPRED
    }
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

#' Order an event table the way the nlmixr2 simulation solves it
#'
#' rxSolve returns its rows in event-table order, so anything that has to line
#' up row-for-row with a solve (`obs` in an nlmixr2 VPC, say) must be put in
#' this same order first. Doses sort ahead of observations at a shared
#' timepoint, hence the descending `EVID`.
#'
#' @param events an event table (data.frame with at least `ID` and `TIME`).
#'
#' @returns `events`, reordered.
#' @noRd
sort_nlmixr_events <- function(events) {
  if("EVID" %in% names(events)) {
    dplyr::arrange(events, .data$ID, .data$TIME, -.data$EVID)
  } else {
    dplyr::arrange(events, .data$ID, .data$TIME)
  }
}

#' Which rows of an event table a solve returns output for
#'
#' Dosing records (`EVID` 1 and 4) and resets (`EVID` 3) advance the system
#' without producing a solved row; observations (`EVID` 0) and other-type
#' events (`EVID` 2) each produce one. Datasets without `EVID` fall back to
#' `MDV`, and then to "every row".
#'
#' Callers that need to align their own rows with a solve should use this
#' rather than guessing at `MDV == 0`: observations flagged `MDV = 1` (BLQ
#' handling and the like) are still solved, and `EVID = 2` records add rows
#' that no `MDV` filter accounts for.
#'
#' @param events an event table.
#'
#' @returns logical vector, one element per row of `events`.
#' @noRd
nlmixr_solved_rows <- function(events) {
  if("EVID" %in% names(events)) return(events$EVID %in% c(0, 2))
  if("MDV" %in% names(events)) return(events$MDV == 0)
  rep(TRUE, nrow(events))
}

#' Solve a model with the between-subject random effects zeroed
#'
#' Gives the population prediction (`PRED`) for a set of events. rxSolve
#' returns either the individual prediction or nothing else in a single call,
#' so `PRED` needs its own solve of the same events through
#' [rxode2::zeroRe()]'d model.
#'
#' Best-effort: returns `NULL` when the installed rxode2 has no `zeroRe()`,
#' when the model has no random effects to zero, or when the solve fails for
#' any other reason. Callers fall back to reporting `IPRED` as `PRED`.
#'
#' @param nlmixr_fn the nlmixr2/rxode2 model function.
#' @param events the event table for one regimen.
#'
#' @returns numeric vector of population predictions, one per solved row, or
#' `NULL`.
#' @noRd
solve_population_prediction <- function(nlmixr_fn, events) {
  if(!rx_zero_re_supported()) return(NULL)
  res <- tryCatch(
    suppressWarnings(suppressMessages(
      rxode2::rxSolve(
        object = rxode2::zeroRe(nlmixr_fn),
        events = events,
        nsim = 1,
        returnType = "data.frame"
      )
    )),
    error = function(e) NULL
  )
  if(is.null(res)) return(NULL)
  ## Pharmpy's nlmixr code declares IPRED in the model block; rxSolve's own
  ## `ipredSim` is the fallback for models that don't.
  col <- intersect(c("IPRED", "ipredSim"), names(res))
  if(!length(col)) return(NULL)
  as.numeric(res[[col[1]]])
}

#' Does the installed rxode2 expose `zeroRe()`?
#'
#' Not present in every rxode2 version. Where it is missing, `PRED` degrades
#' to `IPRED` rather than erroring.
#'
#' @returns `TRUE` when rxode2 exports `zeroRe`.
#' @noRd
rx_zero_re_supported <- function() {
  ns <- tryCatch(asNamespace("rxode2"), error = function(e) NULL)
  if(is.null(ns)) return(FALSE)
  "zeroRe" %in% getNamespaceExports(ns)
}

#' Does the installed rxode2 expose its RNG seed hooks?
#'
#' `rxSetSeed()`/`rxGetSeed()` are not present in every rxode2 version, so the
#' seeding is applied only where both exist.
#'
#' @returns `TRUE` when both hooks are exported by rxode2.
#' @noRd
rx_seed_supported <- function() {
  ns <- tryCatch(asNamespace("rxode2"), error = function(e) NULL)
  if(is.null(ns)) return(FALSE)
  all(c("rxSetSeed", "rxGetSeed") %in% getNamespaceExports(ns))
}
