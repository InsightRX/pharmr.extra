#' Run simulations
#'
#' @inheritParams run_nlme
#' @param model either a Pharmpy model object, or a filename (for a model
#' with NONMEM model code). If the latter, `run_sim()` will attempt to load the 
#' model into Pharmpy first.
#' @param fit a Pharmpy modelfit object.
#' @param regimen if specified, will replace the regimens for each subject with
#' a custom regimen. Can be specified in two ways. The simplest way is to just
#' specify a list with elements `dose`, `interval`, `n`, and
#' `route` (and `t_inf` / `rate` for infusions).
#' E.g. `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
#' Alternatively, regimens can be specified as a data.frame. The data.frame
#' specified all dosing times (`dose`, `time` columns) and `route` and `t_inf` /
#' `rate`. The data.frame may also optionally contain a `regimen` column that
#' specifies a name for the regimen. This can be used to simulate multiple
#' regimens.
#' @param covariates if specified, will replace subjects with subjects specified
#' in a data.frame. In the data.frame, the column names should correspond
#' exactly to any covariates included in the model. An `ID` column is required,
#' and for time-varying covariates, a `TIME` column is also required (otherwise
#' it will be assumed covariates are not changing over time).
#' @param t_obs a vector of observations times. If specified, will override
#' the observations in each subject in the input dataset.
#' @param dictionary a dataset dictionary.
#' @param n_subjects number of subjects to simulate, when using sampled data
#' (i.e. requires `covariates` argument)
#' @param n_iterations number of iterations of the entire simulation to
#' perform. The dataset for the simulation will stay the same between each
#' iterations.
#' @param add_pk_variables calculate basic PK variables that can be extracted
#' in post-processing, such as CMAX_OBS, TMAX_OBS, AUC_SS.
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
    t_obs = NULL,
    dictionary = NULL,
    regimen = NULL,
    covariates = NULL,
    tool = c("auto", "nonmem", "nlmixr2"),
    n_subjects = NULL,
    n_iterations = 1,
    variables = NULL,
    add_pk_variables = FALSE,
    output_file = "simtab",
    update_table = TRUE,
    seed = 12345,
    verbose = TRUE
) {
  
  ## Make sure `data` is passed around as filename, to avoid passing data to Pharmpy (error prone)
  if(is.null(data)) {
    if(inherits(model, "character")) {
      tmp_model <- pharmr::read_model(model)
    } else {
      tmp_model <- model
    }
    data <- tmp_model$dataset
  }
  if(!inherits(data, "character")) {
    datafile <- tempfile(fileext = ".csv")
    write.csv(data, datafile, quote=F, row.names=F)
    data <- datafile
  }
  if (!file.exists(data)) {
    cli::cli_abort("Data file {dataset_file} does not exist")
  }
  input_data <- read.csv(data)
  input_has_column <- list()
  for(key in c("CMT", "EVID", "MDV", "RATE")) {
    input_has_column[[key]] <- key %in% names(input_data)
  }

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
      model <- create_model_from_file(model_file = model, data = data)
      if(inherits(model, "pharmpy.model.model.Model")) {
        cli::cli_alert_info("Model successfully imported as Pharmpy model object.")
      } else {
        cli::cli_abort("Could not load model into Pharmpy. Please check supplied model object or model code.")
      }
    }
  }
  tool <- match.arg(tool)
  if(tool == "auto") {
    if(inherits(model, "pharmpy.model.external.nonmem.model.Model")) {
      tool <- "nonmem"
    }
  }
  if(tool != "nonmem") {
    cli::cli_abort("Sorry, currently only supporting NONMEM simulations.")
  }
  ## make sure we have regimen as a data.frame
  regimen_df <- NULL
  if(!is.null(regimen)) {
    if(inherits(regimen, "data.frame")) {
      regimen_df <- regimen
    } else if (inherits(regimen, "list")) {
      regimen_df <- do.call(create_regimen, args = regimen) |>
        dplyr::mutate(regimen = "regimen 1")
    } else {
      cli::cli_abort("`regimen` needs to be either a data.frame or a list, or NULL.")
    }
  }
  default_dictionary <- list(
    ID = "ID",
    TIME = "TIME",
    DV = "DV",
    EVID = "EVID",
    AMT = "AMT",
    CMT = "CMT",
    MDV = "MDV"
  )
  merge_dictionary <- function(user = list(), default_dictionary) {
    defaults <- default_dictionary
    defaults[names(user)] <- user
    defaults
  }
  dictionary <- merge_dictionary(dictionary, default_dictionary)

  ## Set CMT to NA if not in dataset
  if(!is.null(dictionary$CMT) && !input_has_column[["CMT"]]) {
    input_data[[dictionary$CMT]] <- NA
  }
  if(is.null(covariates)) { # use original dataset
    if(verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
    if(!is.null(dictionary)) {
      for(key in names(dictionary)) {
        if(! dictionary[[key]] %in% names(sim_data)) {
          cli::cli_alert_warning("Dictionary value for {key} ('{dictionary[[key]]}') not found in dataset.")
          dictionary[[key]] <- NULL
        }
        if(key %in% names(sim_data) && key != dictionary[[key]]) {
          sim_data <- sim_data |>
            dplyr::rename(
              !!!rlang::set_names(
                key,
                paste0(key, "_old")
              ))
        }
      }
      sim_data <- sim_data |>
        dplyr::rename(
          !!!rlang::set_names(
            as.character(dictionary),
            names(dictionary)
          )
        )
    }
    if(is.null(n_subjects)) {
      n_subjects <- length(unique(input_data[[dictionary$ID]]))
    } else {
      ids <- unique(sim_data[[dictionary$ID]])
      sim_data <- sim_data |>
        dplyr::filter(.data[[dictionary$ID]] %in% ids[1:n_subjects])
    }
  } else { ## user provided sampled covariates in `covariates`
    if(is.null(n_subjects)) {
      n_subjects <- nrow(covariates)
    }
    if(verbose) cli::cli_alert_info("Preparing sampled dataset for simulation")
    ids <- unique(input_data[[dictionary$ID]])
    random_sample <- sample(ids, n_subjects, replace = TRUE)
    sim_data <- lapply(seq_along(random_sample), function(i) {
      input_data |>
        dplyr::filter(.data[[dictionary$ID]] == random_sample[i]) |>
        dplyr::mutate(!!rlang::sym(dictionary$ID) := i)
    }) %>%
      dplyr::bind_rows()
    if(verbose) cli::cli_alert_info("Updating covariates for subjects in simulation")
    covs_reqd <- unlist(lapply(
      pharmr::get_model_covariates(model),
      function(x) { x$name }
    ))
    if(! all(covs_reqd %in% names(covariates))) {
      missing <- covs_reqd[! covs_reqd %in% names(covariates)]
      cli::cli_alert_warning("Not all required covariates supplied in `covariates` data, missing: {missing}. This could be due to renaming of covariates in $INPUT.")
    }
    if(! dictionary$ID %in% names(covariates)) {
      covariates[[dictionary$ID]] <- 1:nrow(covariates)
    }
    if(! dictionary$TIME %in% names(covariates)) {
      covariates[[dictionary$TIME]] <- 0
    }
    new_covariates <- names(covariates)
    new_covariates <- new_covariates[(! new_covariates %in% c(dictionary[["ID"]], dictionary[["TIME"]])) & new_covariates %in% names(sim_data)]
  
    sim_data_cols <- names(sim_data)
    sim_data <- sim_data |>
      dplyr::select(- dplyr::all_of(new_covariates)) |> ## remove existing covariates
      dplyr::left_join(
        covariates,
        by = c(dictionary[["ID"]], dictionary[["TIME"]])
      ) |>
      dplyr::select(dplyr::all_of(sim_data_cols)) |> # ensure order is kept, after left join
      tidyr::fill(dplyr::all_of(new_covariates), .direction = "downup")
  }
    
  if(!is.null(regimen_df)) {
    if(verbose) cli::cli_alert_info("Creating new regimens for subjects in simulation")
    advan <- get_advan(model)
    doses <- create_dosing_records(
      regimen_df,
      sim_data,
      n_subjects,
      dictionary,
      advan
    )

    ## match type of columns to existing dataset (possibly user-supplied)
    doses <- match_type(doses, sim_data, c("AMT", "RATE", "DV"))
    ## remove old doses and add new
    if("EVID" %in% names(sim_data)) {
      sim_data <- sim_data |>
        dplyr::filter(.data$EVID != 1)
    }
    sim_data <- sim_data |>
      dplyr::bind_rows(doses) |>
      dplyr::arrange(.data$.regimen, .data[[dictionary$ID]], .data[[dictionary$TIME]]) |>
      dplyr::group_by(.data[[dictionary$ID]]) |>
      tidyr::fill(
        tidyselect::everything(),
        .direction = "downup"
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ fill_missing(.x)))
    if(is.null(t_obs)) {
      ## TODO: could be made somewhat smarter, based on e.g. original dataset or
      t_max <- max(sim_data[[dictionary$TIME]]) + round(diff(utils::tail(sim_data[[dictionary$TIME]], 2)))
      t_obs <- seq(0, t_max, 4)
    }
  } else {
    sim_data[[".regimen"]] <- "original regimens"
  }
  if(!is.null(t_obs)) {
    if(verbose) cli::cli_alert_info("Creating new observation records for subjects in simulation")
    obs <- create_obs_records(
      sim_data,
      t_obs,
      n_subjects,
      dictionary,
      model
    )
    ## remove old obs and add new
    obs <- match_type(obs, sim_data, c("AMT", "RATE", "DV"))
    sim_data <- sim_data |>
      dplyr::filter(.data$EVID != 0) |>
      dplyr::bind_rows(obs) |>
      dplyr::arrange(.data$.regimen, .data[[dictionary$ID]], .data[[dictionary$TIME]]) |>
      dplyr::group_by(.data[[dictionary$ID]]) |>
      tidyr::fill(
        dplyr::everything(),
        .direction = "downup"
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ fill_missing(.x)))
  }

  ## Remove CMT, EVID, MDV, RATE column, if not in input dataset
  for(key in names(input_has_column)) {
    if(! input_has_column[[key]]) {
      sim_data[[key]] <- NULL
      input_data[[key]] <- NULL
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
        dplyr::arrange(.data[[dictionary$ID]], .data[[dictionary$TIME]], -.data$EVID)
    } else {
      sim_data_regimen <- sim_data_regimen |>
        dplyr::arrange(.data[[dictionary$ID]], .data[[dictionary$TIME]])
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
        variables <- c(default_variables, get_declared_variables(sim_model))
      }
      checked_variables <- c()
      for(variab in variables) {
        check_var <- check_nm_table_variables(sim_model, variab, throw_error = FALSE)
        if(is.null(check_var)) { # i.e. IPRED is declared as variable and we can safely add to table
          checked_variables <- c(checked_variables, variab)
        }
      }
      variables <- unique(c(checked_variables, parameter_names))
      sim_model <- sim_model |>
        remove_tables_from_model() |>
        add_table_to_model(checked_variables, file = output_file)
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
        if(!is.null(regimen_df)) { # regimen needed for calculation of AUCss
          attr(results, "tables")[[output_file]] <- calc_pk_variables(
            data = attr(results, "tables")[[output_file]],
            regimen = regimen_df |>
              dplyr::filter(regimen == reg_label)
          )
        } else {
          attr(results, "tables")[[output_file]] <- calc_pk_variables(
            data = attr(results, "tables")[[output_file]],
            regimen = NULL
          )
        }
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
  
  ## Ensure only t_obs is outputted
  if(!is.null(t_obs) && "TIME" %in% names(out)) {
    out <- out |>
      dplyr::filter(.data$TIME %in% t_obs)
  }

  if(verbose) cli::cli_alert_success("Done")
  out
}

#' Calculate some basic PK variables from simulated or observed data
#'
#' @param data data.frame in NONMEM format
#' @param dictionary TODO
#' @inheritParams run_sim
#'
#' @returns data.frame
calc_pk_variables <- function(
    data,
    regimen = NULL,
    dictionary = NULL
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
#' @param dictionary TODO
#' @param advan TODO
#' 
create_dosing_records <- function(
    regimen,
    data,
    n_subjects,
    dictionary,
    advan = NULL
) {
  if(is.null(dictionary)) dictionary <- list(ID = "ID")
  id_col <- dictionary$ID
  ids <- unique(data[[id_col]])
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
  ## Rename columns to match dictionary
  for(key in names(dictionary)) {
    if (key %in% names(dose) && dictionary[[key]] != key) {
      dose <- dplyr::rename(dose, !!dictionary[[key]] := !!rlang::sym(key))
    }
  }
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
      dplyr::mutate(!!id_col := ids[i])
  }) |>
    dplyr::bind_rows()
  dose_df
}

#' Create observation records, given a specified t_obs vector
#'
#' @param data TODO
#' @param t_obs TODO
#' @param n_subjects TODO
#' @param dictionary TODO
create_obs_records <- function(
    data,
    t_obs,
    n_subjects,
    dictionary,
    model
) {
  ids <- unique(data[[dictionary$ID]])
  if(length(ids) < n_subjects) {
    ids <- c(ids, max(ids) + 1:(n_subjects-length(ids)))
  }
  unq_reg <- unique(data[[".regimen"]])
  ## create a template row
  ## first try pull CMT from data. if not available in data, try based on ADVAN
  if("MDV" %in% names(data)) {
    cmt <- data |>
      dplyr::filter(.data[[dictionary$ID]] == 1 & .data$EVID == 0 & .data$MDV == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(dictionary$CMT)
  } else {
    cmt <- data |>
      dplyr::filter(.data[[dictionary$ID]] == 1 & .data$EVID == 0) |> 
      dplyr::slice(1) |>
      dplyr::pull(dictionary$CMT)
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
  for(key in names(dictionary)) {
    if (key %in% names(obs) && dictionary[[key]] != key) {
      obs <- dplyr::rename(obs, !!dictionary[[key]] := !!rlang::sym(key))
    }
  }
  id_col <- dictionary$ID
  ## extend single sampling design to multiple subjects
  obs_df <- lapply(1:n_subjects, function(i) {
    obs |>
      dplyr::mutate(!!id_col := ids[i])
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
