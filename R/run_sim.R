#' Run simulations
#'
#' @inheritParams run_nlme
#' @param model either a Pharmpy model object, or a filename (for a model
#' with NONMEM model code). If the latter, `run_sim()` will attempt to load the
#' model into Pharmpy first.
#' @param fit a Pharmpy modelfit object.
#' @param data a NONMEM-format data.frame to use as the simulation dataset.
#' Typically the output of [create_sim_dataset()]. If `NULL`, the dataset
#' attached to `model` is used as-is.
#' @param n_iterations number of iterations of the entire simulation to
#' perform. The dataset for the simulation will stay the same between each
#' iterations.
#' @param add_pk_variables calculate basic PK variables: CMAX_OBS, TMAX_OBS,
#' CMIN_OBS, and (when `CL` is in the output table) AUC_SS. AUC_SS is derived
#' as the last dose in the simulation dataset divided by CL.
#' @param update_table should any existing $TABLE records be removed, and a new
#'  `simtab` be created? This is default. If `FALSE`, it will leave $TABLEs as
#' specifed in the model. However, in the return object, only the first table
#' is returned back. If `FALSE`, the `add_pk_variables` argument will be ignored.
#' @param tool the tool to run the model in, either `nonmem`, or `nlmixr`.
#' @param variables vector of variables to output. If `NULL`, will output
#' default variables `c("ID", "TIME", "DV", "EVID", "PRED")` as well as
#' all variables declared in the NONMEM code.
#' @param output_file TODO
#' @param seed TODO
#'
#' @returns data.frame with simulation results
#'
#' @export
run_sim <- function(
    fit = NULL,
    data = NULL,
    model = NULL,
    id = irxutils::get_random_id("sim_"),
    force = FALSE,
    tool = c("auto", "nonmem", "nlmixr2"),
    n_iterations = 1,
    variables = NULL,
    add_pk_variables = FALSE,
    output_file = "simtab",
    update_table = TRUE,
    seed = 12345,
    verbose = TRUE
) {
  ## parse arguments
  if(is.null(fit) && is.null(model)) {
    cli::cli_abort("For simulations we need either a `fit` object, or a `model` file (with updated estimates)")
  }
  if(is.null(model)) {
    if(!is.null(attr(fit, "final_model"))) {
      model <- attr(fit, "final_model")
    } else {
      cli::cli_abort("No proper model object available. Need either a `model` object or a `fit` object with a model attached.")
    }
  } else {
    if(inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_alert_info("Supplied `model` is a Pharmpy model object.")
    } else {
      cli::cli_alert_info("Supplied `model` is not a Pharmpy model object. Trying to load in Pharmpy.")
      if(!is.null(data) && inherits(data, "data.frame")) {
        data_file <- tempfile(fileext = ".csv")
        write.csv(data, data_file, quote = F, row.names = F)
        model <- create_model_from_file(model_file = model, data = data_file)
      } else {
        model <- create_model_from_file(model_file = model)
      }
      if(inherits(model, "pharmpy.model.model.Model")) {
        cli::cli_alert_info("Model successfully imported as Pharmpy model object.")
      } else {
        cli::cli_abort("Could not load model into Pharmpy. Please check supplied model object or model code.")
      }
    }
  }
  input_data <- model$dataset

  tool <- match.arg(tool)
  if(tool == "auto") {
    if(inherits(model, "pharmpy.model.external.nonmem.model.Model")) {
      tool <- "nonmem"
    }
  }
  if(tool != "nonmem") {
    cli::cli_abort("Sorry, currently only supporting NONMEM simulations.")
  }

  ## Use provided data or fall back to model's dataset
  if(is.null(data)) {
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- as.data.frame(input_data)
    sim_data[[".regimen"]] <- "original regimens"
  } else {
    if(!inherits(data, "data.frame")) {
      cli::cli_abort(
        c("`data` must be a data.frame (typically the output of {.fn create_sim_dataset}).",
          x = "Got an object of class {.cls {class(data)}}.",
          i = "To build a simulation dataset from a file or model, use {.fn create_sim_dataset} first.")
      )
    }
    sim_data <- data
    if(!".regimen" %in% names(sim_data)) {
      sim_data[[".regimen"]] <- "original regimens"
    }
  }

  ## get unique regimens / datasets to simulate
  unique_regimens <- unique(sim_data[[".regimen"]])
  comb <- list()

  ## Loop over regimens to simulate
  for(reg_label in unique_regimens) {

    ## grab data for regimen
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
    
    ## Ensure column names & order matches
    if(all(names(sim_data_regimen) %in% names(input_data))) {
      sim_data_regimen <- sim_data_regimen[, names(input_data)]
    }

    ## Set simulation (pharmr::set_simulation() modifies the model that sometimes invalidate the model, so add manually)
    if(verbose) cli::cli_alert_info("Changing model to simulation-only model")
    sim_model <- model |>
      set_simulation_clean(seed = seed, n = n_iterations)

    ## Add tables
    if(update_table) {
      if(verbose) cli::cli_alert_info("Updating table record(s)")
      parameter_names <- get_defined_pk_parameters(sim_model)
      if(is.null(variables)) {
        default_variables <- c("ID", "TIME", "DV", "EVID", "PRED")
        covariate_names <- vapply(
          pharmr::get_model_covariates(sim_model),
          function(x) x$name,
          character(1)
        )
        variables <- c(default_variables, get_declared_variables(sim_model), covariate_names)
      }
      checked_variables <- c()
      for(variab in variables) {
        check_var <- check_nm_table_variables(sim_model, variab, throw_error = FALSE)
        if(is.null(check_var)) { # i.e. IPRED is declared as variable and we can safely add to table
          checked_variables <- c(checked_variables, variab)
        }
      }
      table_variables <- unique(c(checked_variables, parameter_names))
      sim_model <- sim_model |>
        remove_tables_from_model() |>
        add_table_to_model(table_variables, file = output_file)
    } else {
      if(verbose) cli::cli_alert_info("Using existing table record(s)")
    }

    ## Update dataset (in safe way, avoiding pharmr::set_dataset)
    if(verbose) cli::cli_alert_info("Updating dataset reference")
    new_dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(sim_data_regimen, new_dataset_file, quote = F, row.names = F)
    
    ## Run simulation
    if(verbose) cli::cli_alert_info("Running simulation ({reg_label})")

    ## sim_data_regimen
    results <- run_nlme(
      model = sim_model,
      data = new_dataset_file,
      id = id,
      force = TRUE,
      auto_stack_encounters = FALSE,
      verbose = FALSE
    )

    ## post-processing
    if(update_table) {
      if(add_pk_variables) {
        ## Derive the dosing regimen from sim_data_regimen so AUC_SS can be
        ## computed in calc_pk_variables (needs regimen$dose).
        regimen_for_pk <- NULL
        if("EVID" %in% names(sim_data_regimen) && "AMT" %in% names(sim_data_regimen)) {
          dose_rows <- sim_data_regimen[sim_data_regimen$EVID == 1, , drop = FALSE]
          if(nrow(dose_rows) > 0) {
            regimen_for_pk <- list(dose = dose_rows$AMT)
          }
        }
        attr(results, "tables")[[output_file]] <- calc_pk_variables(
          data = attr(results, "tables")[[output_file]],
          regimen = regimen_for_pk
        )
      }
    }

    ## grab table, return
    if(verbose) cli::cli_alert_info("Exporting simulation results ({reg_label})")
    comb[[reg_label]] <- attr(results, "tables")

  }

  ## combine back down to single data.frame again
  out <- lapply(unique_regimens, function(reg_label) {
    table_names <- names(comb[[reg_label]])
    simtab <- table_names[1]
    if(!is.null(simtab) && !is.null(comb[[reg_label]][[simtab]])) {
      return(
        comb[[reg_label]][[simtab]] |>
          dplyr::mutate(regimen_label = reg_label)
      )
    } else {
      cli::cli_warn("Simulation for {reg_label} did not output any results.")
      return(data.frame())
    }
  }) |>
    dplyr::bind_rows()
  
  if(verbose) cli::cli_alert_success("Done")
  out
}

#' Calculate some basic PK variables from simulated or observed data
#'
#' @param data data.frame in NONMEM format
#' @param run_sim
#'
#' @returns data.frame
calc_pk_variables <- function(
    data,
    regimen = NULL
) {

  if(!is.null(data)) {
    ## Find cmax/tmax for each ID
    data <- data |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(CMAX_OBS = max(.data$DV)) |>
      dplyr::mutate(TMAX_OBS = .data$TIME[match(.data$CMAX_OBS[1], .data$DV)][1])

    ## Find Cmin for each ID, for last interval
    if(all(c("ID", "EVID") %in% names(data))) {
      tmp_data <- data |>
        dplyr::group_by(.data$ID) |>
        dplyr::mutate(.dose_id = cumsum(.data$EVID == 1))
      last_obs_dose_id <- tmp_data |>
        dplyr::filter(.data$EVID == 0) |>
        dplyr::pull(".dose_id") |>
        utils::tail(1)
      cmin_data <- tmp_data |>
        dplyr::mutate(.dose_cmin = max(c(1, last_obs_dose_id))) |> # last full interval (before last dose)
        dplyr::filter(.data$.dose_id == .data$.dose_cmin & .data$EVID == 0) |>
        dplyr::summarise(CMIN_OBS = min(.data$DV))
      data <- dplyr::left_join(data, cmin_data, by = "ID")      
    } else {
      cli::cli_alert_info("Skipping Cmin calculation, some required columns not in output data.")      
    }

    ## Add AUC_SS as CL/dose, if we're simulating a specific regimen
    if(!is.null(regimen) && "CL" %in% names(data)) {
      data <- data |>
        dplyr::mutate(AUC_SS = utils::tail(regimen$dose, 1) / .data$CL)
    }
  }

  data
}

#' Create dosing records, given a specified regimen as a data frame with
#' potentially multiple regimens and varying dosing times / doses
#'
#' @param regimen TODO
#' @param data TODO
#' @param n_subjects TODO
#' @param advan TODO
#' 
create_dosing_records <- function(
    regimen,
    data,
    n_subjects,
    advan = NULL
) {
  ids <- unique(data$ID)
  if(length(ids) < n_subjects) {
    ids <- c(ids, max(ids) + 1:(n_subjects-length(ids)))
  }
  if(!is.null(regimen$regimen)) {
    unq_reg <- unique(regimen$regimen)
  } else {
    regimen$regimen <- "regimen 1"
    unq_reg <- "regimen 1"
  }
  ## logic for picking dosing compartments
  cmt_oral <- 1
  cmt_iv <- 2
  if(!is.null(advan)) {
    if(advan %in% c(1, 3, 11)) {
      cmt_iv <- 1
      if(any(regimen$route %in% c("oral", "im", "sc"))) {
        cli::cli_abort("The model structure does not support oral, im, or sc dosing, only iv.")
      }
    }
  }
  dose <- data.frame(
    ID = 1,
    TIME = regimen$time,
    AMT = regimen$dose,
    EVID = 1,
    MDV = 1,
    DV = 0,
    CMT = 1,
    .regimen = regimen$regimen
  )
  if(is.null(regimen$t_inf)) regimen$t_inf <- 0
  dose$RATE <- 0
  dose$RATE[regimen$t_inf != 0] <- dose$AMT[regimen$t_inf != 0] / regimen$t_inf[regimen$t_inf != 0]
  dose <- dose |>
    dplyr::mutate(CMT = dplyr::case_when(
      regimen$route %in% c("oral", "sc", "im") ~ cmt_oral, # logic for picking dosing cmt
      regimen$route %in% c("iv", "bolus", "infusion") ~ cmt_iv,
      .default = 1
    ))
  dose_df <- lapply(1:n_subjects, function(i) {
    dose |>
      dplyr::mutate(ID = ids[i])
  }) |>
    dplyr::bind_rows()
  dose_df
}

#' Create observation records, given a specified t_obs vector
#'
#' @param data TODO
#' @param t_obs TODO
#' @param n_subjects TODO
create_obs_records <- function(
    data,
    t_obs,
    n_subjects,
    model
) {
  ids <- unique(data$ID)
  if(length(ids) < n_subjects) {
    ids <- c(ids, max(ids) + 1:(n_subjects-length(ids)))
  }
  unq_reg <- unique(data[[".regimen"]])
  ## create a template row
  ## first try pull CMT from data. if not available in data, try based on ADVAN
  if("MDV" %in% names(data)) {
    cmt <- data |>
      dplyr::filter(.data$ID == 1 & .data$EVID == 0 & .data$MDV == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(CMT)
  } else {
    cmt <- data |>
      dplyr::filter(.data$ID == 1 & .data$EVID == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(CMT)
  }
  if(is.null(cmt) || is.na(cmt) || length(cmt) == 0) {
    cmt <- get_obs_compartment(model)
  }
  obs <- data.frame(
    ID = 1,
    TIME = t_obs,
    AMT = 0,
    EVID = 0,
    MDV = 0,
    DV = 0,
    CMT = cmt,
    RATE = 0
  )
  ## extend single sampling design to multiple subjects
  obs_df <- lapply(1:n_subjects, function(i) {
    obs |>
      dplyr::mutate(ID = ids[i])
  }) |>
    dplyr::bind_rows()
  ## extend to multiple regimens, if needed
  obs_df <- lapply(1:length(unq_reg), function(i) {
    obs_df |>
      dplyr::mutate(.regimen = unq_reg[i])
  }) |>
    dplyr::bind_rows()
  obs_df
}

match_type <- function(x, reference, cols = c("AMT", "RATE", "DV")) {
  for(key in cols) {
    if(inherits(reference[[key]], "character")) {
      x[[key]] <- as.character(x[[key]])
    }
  }
  x
}

fill_missing <- function(x, default = NA) {
  if (all(is.na(x))) {
    if(inherits(x, "character")) {
      rep(".", length(x))
    } else {
      rep(0, length(x))
    }
  } else {
    x
  }
}

#' Create a dictionary from given specs and default dictionary as fallback
#' 
parse_data_dictionary <- function(
  dictionary,
  default = list(
    ID = "ID",
    TIME = "TIME",
    DV = "DV",
    EVID = "EVID",
    AMT = "AMT",
    CMT = "CMT",
    MDV = "MDV"
  )
) {
  updated <- default
  updated[names(dictionary)] <- dictionary
  updated
}
