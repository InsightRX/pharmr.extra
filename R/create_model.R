#' Create model
#'
#' This is essentially a wrapper around the model-creation and -modification
#' functionality in pharmr/Pharmpy.
#'
#' @param data filename of dataset or data.frame as input to NONMEM / nlmixr.
#' @param route route of administration, either `oral` or `iv`
#' @param lag_time add a lag time, default is `FALSE`
#' @param n_transit_compartments number of transit-compartments for absorption
#' model. Default is `0`.
#' @param bioavailability Add a bioavailability parameter? Default is `FALSE`.
#' Will add using a logit function.
#' @param n_cmt number of elimination and distribution compartments. Default is
#' 1, i.e. no peripheral distributions.
#' @param elimination elimination type, either `linear` or `michaelis-menten`.
#' @param tmdd_type if a TMDD model structure is desired, can choose one of
#' `full`, `ib`, `crib`, `cr`, `qss`, or `mmapp`. See Pharmpy/pharmr 
#' documentation for details.
#' @param metabolite either `TRUE`/`FALSE` or a `list`. If logical, determines 
#' if a metabolite compartment be added (with input from central using linear 
#' kinetics). In this case, DVID in the dataset should be set to 1 for the 
#' parent compound and 2 for the metabolite. If argument is a `list` object,
#' it will require list elements `dvid_drug` (integer) and `presystemic` 
#' (logical) to be specified. `dvid_drug` specified the index number for 
#' parent drug in the `DVID` column in the dataset. `presystemic` determines
#' whether the metabolite is formed before absorption (`TRUE`), or after
#' systemic absorption (`FALSE`).
#' @param iiv either `character` or a `list` object. If `character`, should be
#' either "basic" (only CL and V parameters) or "all" (IIV on all parameters).
#' If specified as a list object, it should contain the IIV magnitude (on SD
#' scale) for parameters and potential correlations specified using a tilde,
#' e.g. `list("CL" = 0.2, "V" = 0.3, "CL~V" = 0.1)`.
#' @param iiv_type either `character` or `list`. If character, one of
#' `c("exp", "add", "prop", "log", "re_log")`. If `list`, should specify for
#' each parameter the effect type, e.g. `list(CL = "add", V = "exp")`. Default
#' is `"exp"` for all.
#' @param ruv one of `proportional`, `additive`, or `combined`.
#' @param covariates list of parameter-covariate effects, e.g.
#' `list(CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow")`
#' Values in list need to match one of the effects allowed by pharmpy.
#' @param force_ode force creation of a model with ODEs, even though the model
#' is linear. Can be `FALSE` (default), `TRUE`, or ADVAN number (for NONMEM 
#' models). In the latter case, options are either `6`, `9`, or `13`.
#' @param scale_observations scale observations by factor, e.g. due to unit
#' differences between dose and concentration. E.g. `scale_observations = 1000`
#' will add `S1 = V/1000` (for a 1-compartment model) to NONMEM code.
#' @param estimation_method character vector of one or more estimation methods.
#' A single method (e.g. `"foce"`) sets one estimation step; a vector (e.g.
#' `c("saem", "imp")`) creates sequential estimation steps. Available methods:
#' `"fo"`, `"foce"`, `"its"`, `"impmap"`, `"imp"`, `"saem"`.
#' @param estimation_options options for estimation method(s). For a single
#' estimation method, specify a flat list, e.g. `list(NITER = 50)`. For
#' multiple estimation methods, specify a named list of lists where names match
#' the method names, e.g. `list(SAEM = list(NBURN = 500), IMP = list(NITER = 10))`.
#' Methods not listed will use their default options. If a flat list is provided
#' with multiple estimation methods, it applies to the first step only.
#' @param uncertainty_method Compute uncertainty for parameter estimations.
#' One of `sandwich` (default), `smat`, `fmat`, `efim`, or `none`.
#' @param sir_options options for running SIR in covariance step.
#' A list with options `niter` and `samples`. If `niter <= 0` 
#' (default), SIR will not be run.
#' @param blq_method method for handling data below the limit of quantification.
#' Available options are `m1`, `m3`, `m4`, `m5`, `m6`, `m7`, as described
#' by Beal et al. Default is no handling of BLQ data (`NULL`).
#' @param lloq (optional) a numeric value specifying the limit of
#' quantification for observations. Will be disregarded if an `LLOQ` column is
#' in the dataset.
#' @param auto_init automatically update initial estimates to reasonable values
#' based on a crude assessment of the PK data. Default is `TRUE`.
#' @param auto_stack_encounters detects if TIME within an individual is
#' decreasing from one record to another, which NONMEM cannot handle.
#' If this happens, it will add a reset event (EVID=3) at that time, and
#' increase the TIME for subsequent events so that NONMEM does not throw an
#' error. It will increase the time for the next encounter to the maximum
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same
#' individual ispresent but is included in the dataset as time-after-dose and
#' not actual time since first overall dose.
#' @param auto_stack_encounters detects if TIME within an individual is
#' decreasing from one record to another, which NONMEM cannot handle.
#' If this happens, it will add a reset event (EVID=3) at that time, and
#' increase the TIME for subsequent events so that NONMEM does not throw an
#' error. It will increase the time for the next encounter to the maximum
#' encounter length across all subjects in the dataset (rounded up to 100).
#' If no decreasing TIME is detected, nothing will be done (most common case).
#' This feature is useful e.g. for crossover trials when data on the same
#' individual ispresent but is included in the dataset as time-after-dose and
#' not actual time since first overall dose.
#' @param mu_reference Control mu-referencing of the model. `"auto"` (default)
#' applies mu-referencing automatically when `estimation_method = "saem"`.
#' `TRUE` always applies mu-referencing. `FALSE` never applies it.
#' @param settings additional settings for model creation and model estimation.
#' TBD
#' @param tables which pre-specified tables to add, defaults to `parameters`
#' and `fit` tables.
#' @param full_tables For the default tables, should all input columns from be
#' included in the output tables? Default `FALSE`.
#' @param name name of model
#' @param tool output model type, either `nonmem` or `nlmixr`
#' @param verbose verbose output?
#'
#' @returns TODO
#' 
#' @export
create_model <- function(
    route = c("auto", "oral", "sc", "im", "extravascular", "iv"),
    lag_time = FALSE,
    n_transit_compartments = 0,
    bioavailability = FALSE,
    n_cmt = 1,
    elimination = c("linear", "michaelis-menten"),
    iiv = "all",
    iiv_type = "exp",
    tmdd_type = NULL,
    tmdd_dv_types = list(drug = 1, target = 2),
    metabolite = FALSE,
    ruv = c("additive", "proportional", "combined", "ltbs"),
    force_ode = FALSE,
    covariates = NULL,
    scale_observations = NULL,
    data = NULL,
    name = NULL,
    estimation_method = "foce",
    estimation_options = list(),
    uncertainty_method = c("sandwich", "smat", "rmat", "efim", "none"),
    sir_options = list(niter = -1),
    blq_method = NULL,
    lloq = NULL,
    tool = c("nonmem", "nlmixr", "nlmixr2"),
    tables = c("fit"),
    full_tables = FALSE,
    auto_init = TRUE,
    auto_stack_encounters = TRUE,
    mu_reference = "auto",
    settings = list(), # TBD
    verbose = FALSE
) {

  ## Parse arguments
  route <- match.arg(route)
  elimination <- match.arg(elimination)
  ruv <- match.arg(ruv)
  tool <- match.arg(tool)
  allowed_est_methods <- c("fo", "foce", "its", "impmap", "imp", "saem")
  if(any(!tolower(estimation_method) %in% allowed_est_methods)) {
    cli::cli_abort(
      "estimation_method must be one or more of: {paste(allowed_est_methods, collapse=', ')}"
    )
  }
  uncertainty_method <- match.arg(uncertainty_method)
  if(uncertainty_method == "none")
    uncertainty_method <- NULL

  ## identify tool
  if(tool == "nlmixr2") { # pharmpy identifies "nlmixr2" as "nlmixr"
    tool <- "nlmixr"
  }
  if(verbose) cli::cli_alert_info(paste0("Writing model in ", tool, " format"))

  ## Pick route
  if(route == "auto") {
    route <- get_route_from_data(data)
  }

  ## Read base model
  if(verbose) cli::cli_alert_info("Reading base model")
  mod <- pharmr::read_model(
    path = get_template_modelfile(route, n_cmt, force_ode)
  )
  if(!is.logical(force_ode)) {
    if(is.numeric(force_ode) || is.integer(force_ode) || is.character(force_ode)) {
      advan <- as.integer(force_ode)
      if(advan == 9L || advan == 13L) { # default ADVAN for templates is 6, update if needed:
        mod <- update_advan(mod, advan)
      }
    }
  }

  ## Absorption
  if(verbose) cli::cli_alert_info("Parsing absorption model")
  if(lag_time) {
    if(route == "iv") {
      cli::cli_alert_warning("IV administration selected, ignoring `lag_time`")
    } else {
      mod <- pharmr::add_lag_time(mod)
    }
  }
  if(isTRUE(bioavailability)) {
    mod <- mod |>
      pharmr::add_bioavailability(
        add_parameter = TRUE,
        logit_transform = TRUE
      ) |>
      pharmr::set_initial_estimates(list(POP_BIO = 0.5))
  }
  if(n_transit_compartments > 0) {
    mod <- pharmr::set_transit_compartments(mod, n = n_transit_compartments)
  }

  ## Distribution: add peripheral compartments
  if(isFALSE(force_ode)) { ## Pharmpy auto-converts ODE models back to linear in add_peripheral_compartments
    ## TODO: update reading of template models for ODE systems
    if(n_cmt > 1) {
      if(verbose) cli::cli_alert_info("Adding peripheral compartments")
      for(i in 1:(n_cmt-1)) {
        mod <- pharmr::add_peripheral_compartment(mod)
      }
    }
  }

  ## Elimination
  if(elimination == "michaelis-menten") {
    if(verbose) cli::cli_alert_info("Adding Michaelis-Menten elimination")
    mod <- mod |>
      pharmr::set_michaelis_menten_elimination()
  }

  ## Add individual variability
  if(verbose) cli::cli_alert_info("Setting IIV")
  mod <- set_iiv(mod, iiv, iiv_type)

  ## Residual error
  if(verbose) cli::cli_alert_info(paste0("Setting error model to: ", ruv))
  mod <- set_residual_error(mod, ruv)

  ## Covariates
  if(!is.null(covariates)) {
    if(verbose) cli::cli_alert_info("Adding covariates to model")
    mod <- add_covariates_to_model(
      model = mod,
      covariates = covariates,
      data = data
    )
  } else {
    if(verbose) cli::cli_alert_warning("Skipping covariates")
  }

  ## Set scaling
  if(!is.null(scale_observations)) {
    obs_compartment <- get_obs_compartment(mod)
    volume_par <- pharmr::get_central_volume_and_clearance(mod)[[1]]
    mod <- mod |>
      set_compartment_scale(
        compartment = obs_compartment,
        expression = list(
          variable = volume_par,
          scale = scale_observations
        )
      )
  }

  ## set parameter estimates to reasonable values based on data
  if(!is.null(data) && auto_init) {
    if(verbose) cli::cli_alert_info("Setting initial estimates")
    inits <- get_initial_estimates_from_data(
      data,
      n_cmt = n_cmt,
      scale_observations = scale_observations,
      ltbs = (ruv == "ltbs")
    )
    if(length(inits) == 0 || any(is.na(inits)) || any(inits == Inf)) {
      cli::cli_alert_warning("Could not compute initial estimates automatically, please check manually.")
    } else {
      inits <- stats::setNames(inits, paste0("POP_", names(inits)))
      mod <- pharmr::set_initial_estimates(
        model = mod,
        inits = inits
      )
    }
  }

  ## Convert to nlmixr2?
  if(tool == "nlmixr") {
    if(verbose) cli::cli_alert_info("Converting model to nlmixr.")
    mod <- pharmr::convert_model(
      model = mod,
      to_format = "nlmixr"
    )
  }

  ## Estimation method
  steps <- mod$execution_steps$to_dataframe()
  n_steps <- nrow(steps)
  first_method <- tolower(estimation_method[1])
  per_step_options <- parse_estimation_options(
    estimation_method, 
    estimation_options
  )
  if(tool == "nonmem") {
    if(verbose) cli::cli_alert_info("Setting estimation options")
    tool_options <- get_estimation_options(
      tool = tool, 
      estimation_method = first_method, 
      estimation_options = per_step_options[[1]]
    )
  } else {
    tool_options <- list()
    cli::cli_alert_warning(paste0("Skipping estimation options for ", tool, ", since not supported by Pharmpy. Please set manually"))
  }
  if(verbose) cli::cli_alert_info("Setting estimation step(s): {paste(estimation_method, collapse=' -> ')}")
  mod <- pharmr::set_estimation_step(
    mod,
    method = first_method,
    idx = n_steps - 1,
    interaction = TRUE,
    tool_options = tool_options,
    parameter_uncertainty_method = uncertainty_method
  )
  if(length(estimation_method) > 1) {
    mod <- update_estimation_method(
      model = mod, 
      estimation_method = estimation_method,
      uncertainty_method = uncertainty_method,
      per_step_options = per_step_options,
      tool = tool,
      verbose = FALSE
    )
  }
  
  ## Modify model to use SIR in COV step?
  if(tool == "nonmem") {
    mod <- mod |>
      add_sir(options = sir_options)
  }

  ## MU referencing?
  apply_mu <- (isTRUE(mu_reference) ||
    (identical(mu_reference, "auto") && "saem" %in% tolower(estimation_method))) &&
    tool == "nonmem"
  if(apply_mu && !pharmr::has_mu_reference(mod)) {
    mod <- pharmr::mu_reference_model(mod)
  }

  ## Parameter uncertainty?
  if(!is.null(uncertainty_method)) {
    if(verbose) cli::cli_alert_info(paste0("Adding parameter uncertainty step: ", uncertainty_method))
    mod <- pharmr::add_parameter_uncertainty_step(
      mod,
      parameter_uncertainty_method = toupper(uncertainty_method)
    )
  }
  
  ## Convert to TDMDD model?
  if(!is.null(tmdd_type)) {
    allowed_tmdd_types <- c("full", "ib", "crib", "cr", "qss", "mmapp")
    if(!tmdd_type %in% allowed_tmdd_types) {
      cli::cli_abort("Specified `tmdd_type` not allowed, select one of: {allowed_tmdd_types}.")
    }
    cli::cli_alert_info("Converting model into TMDD structure ({tmdd_type}).")
    if(!is.null(tmdd_dv_types)) {
      allowed_dv_types <- c("drug", "target", "complex", "drug_tot", "target_tot")
      if(any(!names(tmdd_dv_types) %in% allowed_dv_types)) {
        cli::cli_abort("Specified TMDD dv_types not allowed. Pick from: {allowed_dv_types}.")
      }
      for(key in names(tmdd_dv_types)) { # pharmpy is picky about integer type
        tmdd_dv_types[[key]] <- as.integer(tmdd_dv_types[[key]])
      }
    }
    mod <- mod |>
      pharmr::set_tmdd(
        type = tmdd_type, 
        dv_types = tmdd_dv_types
      )
  }
  
  ## Add metabolite compartment?
  mod <- add_metabolite_compartment(mod, metabolite, route)
  
  ## Add $TABLEs
  if(tool == "nonmem") {
    if(!is.null(tables)) {
      mod <- add_default_output_tables(
        model = mod,
        iiv = iiv,
        tables = tables,
        full_tables = full_tables
      )
    }
  }

  ## Add dataset (needed if we want to add covariates to the model)
  if(!is.null(data)) {
    if(inherits(data, "character")) {
      if(isTRUE(auto_stack_encounters)) {
        cli::cli_warn("`auto_stack_encounters` can only be used when `data` is specified as data.frame, not when it is a CSV filename.")
      }
      if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset.")
      mod <- mod |>
        pharmr::unload_dataset() |>
        pharmr::set_dataset(
          path_or_df = load_data_wrapper(data),
          datatype = "nonmem"
        )
      if(verbose) cli::cli_alert_info("`data` provided as filename: skipping check of dataset and using as-is.")
    } else {
      if(isTRUE(auto_stack_encounters)) {
        data <- stack_encounters(
          data = data,
          verbose = verbose
        )
      }
      if(verbose) cli::cli_alert_info("Updating model dataset with provided dataset.")
      mod <- mod |>
        pharmr::unload_dataset() |>
        pharmr::set_dataset(
          path_or_df = data,
          datatype = "nonmem"
        )
      if(verbose) cli::cli_alert_info("Checking and cleaning dataset.")
      mod <- clean_modelfit_data(
        model = mod,
        try_make_numeric = TRUE
      ) 
    }
  }
  
  ## Handle BLQ
  if(!is.null(blq_method)) {
    blq_method <- tolower(blq_method)
    allowed_blq <- paste0("m", 1:7)
    if(! blq_method %in% allowed_blq) {
      cli::cli_abort("`blq_method` should be one of {allowed_blq}, or `NULL`.")
    }
    if(!is.null(lloq) && "LLOQ" %in% names(mod$dataset)) {
      lloq <- NULL
      cli::cli_alert_info("`lloq` argument cannot be used when `LLOQ` column exists in dataset. Ignoring argument.")
    }
    if(is.null(lloq) && ! "LLOQ" %in% names(mod$dataset) && blq_method %in% c("m2", "m3", "m4", "m5", "m6")) {
      cli::cli_abort("For {blq_method}-method, need either `lloq` argument or a LLOQ column in the dataset.")
    }
    mod <- pharmr::transform_blq(
      model = mod, 
      method = blq_method, 
      lloq = lloq
    )
  }

  ## Set name?
  if(!is.null(name)) {
    mod <- pharmr::set_name(
      mod, 
      new_name = name
    )
  }

  if(verbose) cli::cli_alert_success("Done")

  return(mod)
}

#' Helper function to combine default estimation options with user-specified,
#' and ensure correct format.
#'
#' @param tool TODO
#' @param estimation_method TODO
#' @param estimation_options TODO
get_estimation_options <- function(tool, estimation_method, estimation_options) {
  tool_options <- estimation_options_defaults[[tool]][[estimation_method]]
  if(is.null(tool_options)) tool_options <- list()
  if(!is.null(estimation_options)) {
    tool_options[names(estimation_options)] <- estimation_options
  }
  for(key in names(tool_options)) { # to avoid e.g. `ITER=500.0`
    tool_options[[key]] <- as.character(tool_options[[key]])
  }
  tool_options
}


#' Parse estimation_options into a per-step list.
#'
#' @param estimation_method character vector of estimation method(s)
#' @param estimation_options flat list or named list of lists
#'
#' @returns a list of length `length(estimation_method)`, where each element
#' is the options list for that step (possibly empty).
parse_estimation_options <- function(estimation_method, estimation_options) {
  n_methods <- length(estimation_method)

  # Detect nested (multi-step) format: named list where all values are lists
  is_nested <- is.list(estimation_options) &&
    length(estimation_options) > 0 &&
    all(sapply(estimation_options, is.list))

  if(is_nested) {
    if(n_methods == 1) {
      cli::cli_abort(
        paste0(
          "`estimation_options` was specified as a list of lists, but only one ",
          "`estimation_method` was provided. Use a flat list instead, e.g. `list(NITER = 50)`."
        )
      )
    }
    methods_upper <- toupper(estimation_method)
    opts_upper <- toupper(names(estimation_options))
    unknown <- setdiff(opts_upper, methods_upper)
    if(length(unknown) > 0) {
      cli::cli_abort(
        paste0(
          "Unknown method name(s) in `estimation_options`: ",
          paste(unknown, collapse = ", "),
          ". Must be one of: ",
          paste(methods_upper, collapse = ", "), "."
        )
      )
    }
    lapply(seq_along(estimation_method), function(i) {
      idx <- which(opts_upper == methods_upper[i])
      if(length(idx) > 0) estimation_options[[idx[1]]] else list()
    })
  } else {
    if(n_methods > 1 && length(estimation_options) > 0) {
      warning(
        "`estimation_options` is a flat list but multiple `estimation_method`s were provided. ",
        "Options will apply to the first step only. ",
        "To specify options per step, use a named list of lists, ",
        "e.g. `list(SAEM = list(NBURN = 500), IMP = list(NITER = 10))`.",
        call. = FALSE
      )
    }
    c(list(estimation_options), rep(list(list()), n_methods - 1))
  }
}

#' List of default options for estimation method.
#'
estimation_options_defaults <- list(
  "nonmem" = list(
    "foce" = list(
      MAXEVAL = 2000,
      PRINT = 5,
      POSTHOC = "",
      NOABORT = ""
    ),
    "saem" = list(
      NBURN = 500,
      NITER = 1000,
      ISAMPLE = 2
    )
  ),
  "nlmixr" = list( ## leave empty for now, pharmpy does not support nlmixr options yet.
    "foce" = list( # https://nlmixr2.org/reference/foceiControl.html
    ),
    "saem" = list(
    )
  )
)

#' Add a metabolite compartment, if requested
#' 
add_metabolite_compartment <- function(
  mod,
  metabolite = FALSE,
  route
) {
  if(inherits(metabolite, "logical")) {
    if(isTRUE(metabolite)) {
      cli::cli_alert_info("Adding metabolite compartment.")
      mod <- mod |>
        pharmr::add_metabolite(
          drug_dvid = 1, 
          presystemic = FALSE
        )
    }
  } else if(inherits(metabolite, "list")) {
    if (! all(c("drug_dvid", "presystemic") %in% names(metabolite))) {
      cli::cli_abort("When `metabolite` is specified as a list object, need elements `drug_dvid` and `presystemic`.")
    }
    if (route == "iv" && isTRUE(metabolite$presystemic)) {
      cli::cli_abort("Cannot add presystemic metabolite to a model with route `iv`. Please synchronize `route` and `metabolite` arguments.")
    }
    cli::cli_alert_info("Adding metabolite compartment.")
    mod <- mod |>
      pharmr::add_metabolite(
        drug_dvid = metabolite$drug_dvid,
        presystemic = metabolite$presystemic
      )
  } else {
    cli::cli_abort("`metabolite` argument needs to be either logical or list object.")
  }
  mod
}

#' Logic to set the residual error model structure for the model
#'
#' @param mod TODO
#' @param ruv TODO
set_residual_error <- function(mod, ruv) {
  if(ruv == "proportional") {
    mod <- pharmr::set_proportional_error_model(mod)
  } else if (ruv == "additive") {
    mod <- pharmr::set_additive_error_model(mod)
  } else if (ruv == "combined") {
    mod <- pharmr::set_combined_error_model(mod)
  } else if (ruv == "ltbs") {
    # Pharmpy: first need to make additive, then set to proportional + log-transf.
    mod <- mod |>
      pharmr::set_additive_error_model() |>
      pharmr::set_proportional_error_model(data_trans="log(Y)")
  } else {
    cli::cli_abort("Requested error model structure not recognized.")
  }
  mod
}

#' Get route from data.
#' If dose and observation events all happen in the same compartment,
#' then assume IV administration, else oral absorption (or sc, im, etc).
#'
#' @inheritParams create_model
#' 
#' @param default default route (`iv` by default)
#' 
#' @returns route (character)
#' 
get_route_from_data <- function(data, default = "iv") {
  if(is.null(data)) {
    return(default)
  }
  dataset <- load_data_wrapper(data)
  dose_cmt <- dataset |>
    dplyr::filter(.data$EVID == 1) |>
    dplyr::pull("CMT") |>
    unique()
  obs_cmt <- dataset |>
    dplyr::filter(.data$EVID == 0) |>
    dplyr::pull("CMT") |>
    unique()
  if(length(setdiff(dose_cmt, obs_cmt)) > 0) {
    route <- "oral"
  } else {
    route <- "iv"
  }
  route
}

#' Helper function to get the name of the template modelfile to load,
#' based on route and ODE / analytical
#' 
#' @inheritParams create_model
#' 
#' @returns modelfile name (character)
#' 
get_template_modelfile <- function(route, n_cmt, force_ode) {
  advan_flag <- ""
  if(is.logical(force_ode)) {
    if(force_ode) force_ode <- 6
  }
  if(is.numeric(force_ode) || is.integer(force_ode) || is.character(force_ode)) {
    force_ode <- suppressWarnings(as.integer(force_ode))
    if(is.na(force_ode)) {
      cli::cli_abort("`force_ode` must be TRUE, FALSE, or a numeric ADVAN number (6, 9, or 13).")
    }
    if(force_ode %in% c(6, 9, 13)) {
      advan_flag <- "_ode"
    } else {
      cli::cli_abort("`force_ode` can only be TRUE, FALSE, 6, 9, or 13.")
    }
  }
  use_ode <- isTRUE(force_ode) || (!is.logical(force_ode) && identical(advan_flag, "_ode"))
  base <- "base"
  if(use_ode) {
    if(n_cmt >= 2) base <- paste0(n_cmt, "cmt")
  }
  template_path <- paste0(
    "models/nonmem/", base, "_", 
    ifelse(route == "iv", "iv", "oral"),
    advan_flag,
    ".mod")
  template <- system.file(
    template_path,
    package = "pharmr.extra"
  )
  if(template == "") cli::cli_abort("Can't find template modelfile at {template_path}.")
  template
}
