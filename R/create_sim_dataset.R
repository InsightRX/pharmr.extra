#' Create a NONMEM dataset for simulation
#'
#' Prepares a dataset for use with [run_sim()], handling covariate sampling,
#' regimen replacement, and observation record creation. The returned
#' data.frame can be passed directly to [run_sim()] as the `data` argument.
#'
#' @param model a Pharmpy model object, or a path to a NONMEM model file
#' (`.mod`). If a file path is supplied, the model is loaded with
#' `pharmr::read_model()` so that the `$DATA` path can be resolved.
#' @param data optional data.frame (or path to a CSV file) to use as the base
#' dataset instead of the dataset attached to `model`. Useful when you want to
#' apply `t_obs` or `regimen` changes to an already-prepared dataset. 
#' It is assumed that the column names in the dataset match the *order* of the 
#' columns in $INPUT in the model. If this is not the case, the creation of
#' the dataset may fail, or the simulations from the dataset may fail.
#' @param regimen if specified, will replace the regimens for each subject with
#' a custom regimen. Can be specified in two ways. The simplest way is to just
#' specify a list with elements `dose`, `interval`, `n`, and
#' `route` (and `t_inf` / `rate` for infusions).
#' E.g. `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
#' An optional `per` element names a covariate column in the dataset; each
#' subject's dose is then multiplied by their value of that column, enabling
#' weight- or BSA-based dosing. E.g.
#' `regimen = list(dose = 5, per = "WT", interval = 24, n = 5, route = "sc")`
#' gives a 5 mg/kg dose using the `WT` column.
#' Alternatively, regimens can be specified as a data.frame. The data.frame
#' specifies all dosing times (`dose`, `time` columns) and `route` and
#' `t_inf` / `rate`. The data.frame may also optionally contain a `regimen`
#' column that specifies a name for the regimen. This can be used to simulate
#' multiple regimens.
#' A function may also be supplied. It will be called once per subject with a
#' data.frame of that subject's rows, and must return a named list accepted by
#' [create_regimen()] (`dose`, `interval`, `n`, `route`; optionally `t_inf`,
#' `per`, `regimen`). This enables fully custom per-subject dosing logic such
#' as tiered weight-band dosing.
#' @param covariates if specified, will replace subjects with subjects specified
#' in a data.frame. In the data.frame, the column names should correspond
#' exactly to any covariates included in the model. An `ID` column is optional;
#' if absent, IDs are generated as `1:nrow(covariates)`. For time-varying
#' covariates, a `TIME` column is also required (otherwise it will be assumed
#' covariates are not changing over time).
#' @param t_obs a vector of observation times. If specified, will override
#' the observations in each subject in the input dataset.
#' @param n_subjects number of subjects to simulate, when using sampled data
#' (i.e. requires `covariates` argument)
#' @param verbose logical; print progress messages.
#'
#' @returns data.frame with a NONMEM-format simulation dataset. A `.regimen`
#'   column is included and is used internally by [run_sim()] to loop over
#'   multiple dosing regimens.
#'
#' @examples
#' \dontrun{
#' model <- pharmr::read_model("run1.mod")
#'
#' # Basic: use the model's original dataset with custom observation times
#' sim_dat <- create_sim_dataset(
#'   model = model,
#'   t_obs = seq(0, 168, by = 4)
#' )
#'
#' # Replace regimen with a flat 500 mg oral dose every 12 h for 5 doses
#' sim_dat <- create_sim_dataset(
#'   model  = model,
#'   regimen = list(dose = 500, interval = 12, n = 5, route = "oral"),
#'   t_obs  = seq(0, 72, by = 2)
#' )
#'
#' # Weight-based dosing (5 mg/kg) using the `per` element —
#' # requires a WT column in the dataset
#' sim_dat <- create_sim_dataset(
#'   model   = model,
#'   regimen = list(dose = 5, per = "WT", interval = 24, n = 3, route = "sc"),
#'   t_obs   = seq(0, 72, by = 4)
#' )
#'
#' # Tiered weight-band dosing via a function
#' dose_fn <- function(x) {
#'   dose <- if (x$WT[1] < 40) 100 else if (x$WT[1] < 80) 200 else 250
#'   list(dose = dose, interval = 14 * 24, route = "sc", n = 6)
#' }
#' sim_dat <- create_sim_dataset(
#'   model   = model,
#'   regimen = dose_fn,
#'   t_obs   = seq(0, 84 * 24, by = 24)
#' )
#'
#' # Simulate with sampled covariates from an external data.frame
#' covs <- data.frame(WT = c(55, 72, 88), AGE = c(34, 51, 67))
#' sim_dat <- create_sim_dataset(
#'   model      = model,
#'   covariates = covs,
#'   regimen    = list(dose = 500, interval = 12, n = 5, route = "oral"),
#'   t_obs      = seq(0, 72, by = 2)
#' )
#'
#' # Simulate multiple regimens for comparison
#' regimens <- combine_regimens(
#'   "low"  = list(create_regimen(dose = 250, interval = 12, n = 5, route = "oral")),
#'   "high" = list(create_regimen(dose = 500, interval = 12, n = 5, route = "oral"))
#' )
#' sim_dat <- create_sim_dataset(
#'   model   = model,
#'   regimen = regimens,
#'   t_obs   = seq(0, 72, by = 2)
#' )
#' }
#'
#' @export
create_sim_dataset <- function(
    model,
    data = NULL,
    regimen = NULL,
    t_obs = NULL,
    covariates = NULL,
    n_subjects = NULL,
    input_from_data = FALSE,
    verbose = TRUE
) {
  if (!inherits(model, "pharmpy.model.model.Model")) {
    if (!inherits(model, "character")) {
      cli::cli_abort("`model` must be a Pharmpy model object or a path to a model file.")
    }
    if (!file.exists(model)) {
      cli::cli_abort("Model file {model} does not exist.")
    }
    model <- create_model_from_file(model_file = model)
    if (!inherits(model, "pharmpy.model.model.Model")) {
      cli::cli_abort("Could not load model into Pharmpy. Please check the supplied model file.")
    }
  }
  if (!is.null(data)) {
    idx <- get_required_input_variables(model, data)
    if (inherits(data, "character")) {
      if (!file.exists(data)) cli::cli_abort("Data file {data} does not exist.")
      input_data <- utils::read.csv(data, check.names = FALSE)
    } else {
      input_data <- as.data.frame(data)
    }
    n_data   <- length(names(input_data))
    n_input  <- length(idx$nonmem_name)
    addl_cols <- n_data - n_input
    if(addl_cols > 0) {
      names(input_data)[seq_len(n_input)] <- idx$nonmem_name
      cli::cli_warn("Number of columns for input dataset is higher than number of columns in $INPUT. Please check dataset and $INPUT correctness. Will continue, assuming extra columns are not needed.")
    } else if (addl_cols < 0) {
      cli::cli_abort("Number of columns for input dataset is lower than number of columns in $INPUT. Please check dataset and $INPUT. Cannot continue creating dataset.")
    }
  } else {
    input_data <- as.data.frame(model$dataset)
  }
  
  if (!"ID" %in% names(input_data)) {
    cli::cli_abort(
      c("Column `ID` not found in the dataset.",
        i = "Available columns: {paste(names(input_data), collapse = ', ')}")
    )
  }

  ## Resolve the actual column name for EVID in the (possibly renamed) dataset.
  ## $INPUT may use a non-standard label (e.g. EVIDX) for the EVID position, in
  ## which case idx$nonmem_name != "EVID" but idx$data_col == "EVID".
  evid_col <- if (exists("idx", inherits = FALSE) && "EVID" %in% idx$data_col) {
    idx$nonmem_name[idx$data_col == "EVID"][1]
  } else {
    "EVID"
  }
  ## Normalize to "EVID" so that downstream helpers (create_dosing_records,
  ## create_obs_records) which use the hardcoded name "EVID" stay consistent.
  ## Without this, bind_rows() would create two separate EVID-like columns
  ## (e.g. "EVIDX" and "EVID"), and fill() would propagate EVIDX=0 into dose
  ## records, causing the later non-zero EVID filter to drop them and leaving
  ## only covariate-free obs records — which fill_missing() then zeroes out.
  if (evid_col != "EVID" && evid_col %in% names(input_data)) {
    names(input_data)[names(input_data) == evid_col] <- "EVID"
    evid_col <- "EVID"
  }

  input_has_column <- list()
  for (key in c("CMT", "EVID", "MDV", "RATE")) {
    input_has_column[[key]] <- key %in% names(input_data)
  }

  ## make sure we have regimen as a data.frame
  regimen_df <- NULL
  if (!is.null(regimen)) {
    if (inherits(regimen, "data.frame")) {
      regimen_df <- regimen
    } else if (inherits(regimen, "list")) {
      regimen_df <- do.call(create_regimen, args = regimen) |>
        dplyr::mutate(regimen = "regimen 1")
    } else if (!inherits(regimen, "function")) {
      cli::cli_abort("`regimen` needs to be a data.frame, a list, a function, or NULL.")
    }
  }

  ## Set CMT to NA if not in dataset
  if (!input_has_column[["CMT"]]) {
    input_data$CMT <- NA
  }

  if (is.null(covariates)) {
    if (verbose) cli::cli_alert_info("Using input dataset for simulation")
    sim_data <- input_data
    if (is.null(n_subjects)) {
      n_subjects <- length(unique(input_data$ID))
    } else {
      ids <- unique(sim_data$ID)
      sim_data <- sim_data |>
        dplyr::filter(.data$ID %in% ids[1:n_subjects])
    }
  } else {
    if (is.null(n_subjects)) {
      n_subjects <- nrow(covariates)
    }
    if (!"ID" %in% names(covariates)) {
      covariates$ID <- seq_len(nrow(covariates))
    }
    if (verbose) cli::cli_alert_info("Preparing sampled dataset for simulation")
    ids <- unique(input_data$ID)
    random_sample <- sample(ids, n_subjects, replace = TRUE)
    sim_data <- lapply(seq_along(random_sample), function(i) {
      input_data |>
        dplyr::filter(.data$ID == random_sample[i]) |>
        dplyr::mutate(ID := i)
    }) |>
      dplyr::bind_rows()
    cov_ids <- unique(covariates$ID)
    covariates <- covariates |>
      dplyr::mutate(ID = match(ID, cov_ids))
    if (verbose) cli::cli_alert_info("Updating covariates for subjects in simulation")
    covs_reqd <- unlist(lapply(
      pharmr::get_model_covariates(model),
      function(x) { x$name }
    ))
    if (!all(covs_reqd %in% names(covariates))) {
      missing <- covs_reqd[!covs_reqd %in% names(covariates)]
      cli::cli_abort(
        "Not all required covariates supplied in `covariates` data, missing: {missing}. \\
        This could be due to renaming of covariates in $INPUT."
      )
    }
    new_covariates <- names(covariates)
    new_covariates <- new_covariates[new_covariates != "ID" & new_covariates %in% names(sim_data)]
    if (verbose) cli::cli_alert_info("Updating covariates: {new_covariates}")

    sim_data_cols <- names(sim_data)
    sim_data <- sim_data |>
      dplyr::select(-dplyr::all_of(new_covariates)) |>
      dplyr::left_join(covariates, by = "ID") |>
      dplyr::select(dplyr::all_of(sim_data_cols)) |>
      tidyr::fill(dplyr::all_of(new_covariates), .direction = "downup")
  }

  if (!is.null(regimen_df) || inherits(regimen, "function")) {
    if (verbose) cli::cli_alert_info("Creating new regimens for subjects in simulation")
    advan <- get_advan(model)
    if (inherits(regimen, "function")) {
      ids <- unique(sim_data$ID)
      doses <- lapply(ids, function(id) {
        subj_data <- sim_data[sim_data$ID == id, , drop = FALSE]
        reg_list  <- regimen(subj_data)
        reg_label <- if (!is.null(reg_list$regimen)) reg_list$regimen else "regimen 1"
        reg_args  <- reg_list[names(reg_list) != "regimen"]
        reg_df    <- do.call(create_regimen, args = reg_args) |>
          dplyr::mutate(regimen = reg_label)
        create_dosing_records(reg_df, subj_data, n_subjects = 1, advan)
      }) |>
        dplyr::bind_rows()
    } else {
      doses <- create_dosing_records(regimen_df, sim_data, n_subjects, advan)
    }
    doses <- match_type(doses, sim_data, c("AMT", "RATE", "DV"))
    if ("EVID" %in% names(sim_data)) {
      sim_data <- sim_data |>
        dplyr::filter(!.data$EVID %in% c(1L, 3L, 4L))
    }
    sim_data <- sim_data |>
      dplyr::bind_rows(doses) |>
      dplyr::arrange(.data$.regimen, .data$ID, .data$TIME) |>
      dplyr::group_by(.data$ID) |>
      tidyr::fill(tidyselect::everything(), .direction = "downup") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ fill_missing(.x)))
    if (is.null(t_obs)) {
      t_max <- max(sim_data$TIME) + round(diff(utils::tail(sim_data$TIME, 2)))
      t_obs <- seq(0, t_max, 4)
    }
  } else {
    sim_data[[".regimen"]] <- "original regimens"
  }

  if (!is.null(t_obs)) {
    if (verbose) cli::cli_alert_info("Creating new observation records for subjects in simulation")
    obs <- create_obs_records(sim_data, t_obs, n_subjects, model)
    obs <- match_type(obs, sim_data, c("AMT", "RATE", "DV"))
    sim_data <- sim_data |>
      dplyr::filter(.data$EVID != 0) |>
      dplyr::bind_rows(obs) |>
      dplyr::arrange(.data$.regimen, .data$ID, .data$TIME) |>
      dplyr::group_by(.data$ID) |>
      tidyr::fill(dplyr::everything(), .direction = "downup") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ fill_missing(.x)))
  }

  ## Remove CMT, EVID, MDV, RATE columns if not in original dataset
  for (key in names(input_has_column)) {
    if (!input_has_column[[key]]) {
      sim_data[[key]] <- NULL
    }
  }

  sim_data
}
