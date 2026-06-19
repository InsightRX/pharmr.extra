#' Clean / check the dataset so Pharmpy doesn't crash on character / date
#' columns
#'
#' @inheritParams run_nlme
#' @param data a data.frame (if not specified, will use `dataset` object in 
#' `model`).
#' @param try_make_numeric should function try to turn character columns
#' into numeric columns? If `FALSE` will just set all values to 0 (but
#' retain column to avoid issues).
#'
#' @returns TODO
#' 
#' @export
clean_modelfit_data <- function(
  model,
  try_make_numeric = TRUE,
  data = NULL,
  verbose = TRUE
) {

  tool <- ifelse(inherits(model, "pharmpy.model.external.nonmem.model.Model"), "nonmem", "nlmixr")

  if(is.null(data)) {
    data <- model$dataset
  }
  if(any(lapply(data, class) != "character")) {
    for(key in names(data)) {
      if(inherits(data[[key]], "character")) {
          if(try_make_numeric) {
            if (key == "DV") {
              ## special handling, if DV includes BLQ values such as "<0.01"
              ## then try to convert to numeric
              idx <- stringr::str_detect(data$DV, "\\<")
              if(any(idx)) { ## add an LLOQ column
                if(verbose)
                  cli::cli_alert_warning("Detected `<` in DV column, adding LLOQ column to handle BLOQ data.")
                data <- dplyr::mutate(
                  data,
                  DV = as.numeric(stringr::str_replace_all(.data$DV, "\\<", "")),
                  LLOQ = dplyr::if_else(idx, .data$DV, 0),
                  DV = dplyr::if_else(idx, 0, .data$DV)
                )
              }
              tmp_dv <- as.numeric(data$DV)
            } else {
              if(verbose)
                cli::cli_alert_warning("Detected character column ({key}), trying to convert to numeric.")
              suppressWarnings({
                data[[key]] <- as.numeric(data[[key]])
              })
            }
          } else {
            data[[key]] <- 0
          }
      }
      # make sure all NAs are set to 0
      if(any(is.na(data[[key]]))) {
        data[[key]][is.na(data[[key]])] <- 0
      }
    }

    if(tool != "nonmem") { ## nlmixr2 requires lower-case `cmt`
      data <- dplyr::rename(data, cmt = "CMT")
    }

    ## Save dataset
    # TODO: pharmr::set_dataset() accepts a data.frame, so why are we saving
    # to a temporary csv? We should simplify.
    # RK: This is a workaround. The simple route currently doesn't properly
    #     load the dataset with proper datainfo in Pharmpy.
    dataset_file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(data, dataset_file, quote = F, row.names = F)

    ## Update dataset in model. `set_dataset()` already reads the CSV and stores
    ## both the dataset and a properly typed (nonmem) datainfo on the model, so an
    ## explicit `load_dataset()` is redundant. It is also unsafe for nlmixr2
    ## models: their datainfo carries no on-disk path (the dataset is held in
    ## memory), so `load_dataset()` re-reading from `datainfo.path` aborts with
    ## "datainfo.path is None". Only fall back to `load_dataset()` if, for some
    ## reason, the dataset did not get attached.
    model <- pharmr::set_dataset(
      model,
      path_or_df = dataset_file,
      datatype = "nonmem"
    )
    if(is.null(model$dataset)) {
      model <- pharmr::load_dataset(model)
    }

    ## Check if model is using log-transform-both-sides
    ## Must happen AFTER set_dataset(), which resets $INPUT and datainfo.
    ltbs <- is_ltbs_model(model)
    if(ltbs) {
      if("LNDV" %in% names(data)) {
        cli::cli_alert_info("Log-transform both sides error model, and detected LNDV column in dataset. Updating $INPUT to use LNDV as the dependent variable (DV=LNDV).")
        model <- model |>
          set_dv("LNDV")
      } else {
        cli::cli_alert_warning("Log-transform both sides error model, but no `LNDV` column. Assuming `DV` is log-transformed.")
      }
    }
  }
  model
}
