#' Compare NLME output from last n runs
#' 
#' The function will scan through the folder and look for 
#' modelfit folders (using `filter`). It will then order these
#' folders by date and select the last `n`. It will then 
#' generate comparison tables for the runs, one for general run info,
#' and one for the parameter estimates.
#' 
#' @param folder folder to scan for NLME results
#' @param filter the filter to apply to scan for results
#' @param n the last n results to compare
#' @param save_info save general run info as a csv file
#' @param save_parameters save run parameters as a csv file
#' 
#' @returns TODO
#' 
#' @export
compare_nlme_runs <- function(
    filter = "run",
    folder = ".",
    n = 3,
    save_info = NULL,
    save_parameters = NULL
) {
  all_items <- list.files(path = folder, full.names = TRUE)
  subfolders <- all_items[file.info(all_items)$isdir]
  subfolders <- subfolders[stringr::str_detect(subfolders, filter)]
  
  # Get creation time (birthtime) and sort
  info <- file.info(subfolders)
  info <- info[order(info$ctime, decreasing = TRUE), ] |>
    dplyr::slice(1:n)
  run_folders <- rownames(info)
  
  # Extract info for each run, detecting engine from the run folder layout.
  # NONMEM runs have run.mod + run.lst; nlmixr2 runs have run.R and the fit
  # object is saved alongside the folder as <id>.rds.
  fits <- lapply(run_folders, function(x) {
    nm_model_file <- file.path(x, "run.mod")
    nl_model_file <- file.path(x, "run.R")
    if(file.exists(nm_model_file)) {
      output_file <- file.path(x, "run.lst")
      fit <- pharmr::read_modelfit_results(
        esttool = "nonmem",
        path = nm_model_file
      )
      fit_info <- get_fit_info(
        fit,
        path = folder,
        output_file = output_file
      )
      attr(fit, "info") <- fit_info
      model <- pharmr::read_model(path = nm_model_file)
      model <- pharmr::set_name(model, basename(x))
      attr(fit, "model") <- model
      return(fit)
    }
    if(file.exists(nl_model_file)) {
      ## nlmixr2 runs persist the fit object as <id>.rds in the parent
      ## folder (see run_nlme_nlmixr()'s save_fit handling).
      rds_path <- file.path(folder, paste0(basename(x), ".rds"))
      if(!file.exists(rds_path)) {
        cli::cli_warn(
          "Skipping {.path {x}}: nlmixr run folder has no saved fit at {.path {rds_path}}."
        )
        return(NULL)
      }
      fit <- readRDS(rds_path)
      ## The pharmpy model attribute is a Python reference that doesn't
      ## survive saveRDS/readRDS — record the run name as a plain
      ## attribute so the comparison table can label the column without
      ## touching the broken Python pointer.
      attr(fit, "run_name") <- basename(x)
      return(fit)
    }
    cli::cli_warn(
      "Skipping {.path {x}}: no run.mod or run.R found."
    )
    NULL
  })
  fits <- fits[!vapply(fits, is.null, logical(1))]
  comp <- compare_nlme_fit(
    fits, 
    return_object = T
  )
  if(!is.null(save_info)) {
    if(!stringr::str_detect(tolower(save_info), "\\.csv$")) {
      cli::cli_abort("Output file needs to be csv.")
    }
    write.csv(comp$info_comb, save_info, quote=F, row.names=T)
  }
  if(!is.null(save_parameters)) {
    if(!stringr::str_detect(tolower(save_parameters), "\\.csv$")) {
      cli::cli_abort("Output file needs to be csv.")
    }
    write.csv(comp$par_comb, save_parameters, quote=F, row.names=T)
  }
  
  ## If no requests to save output, just return the combined object
  if(is.null(save_info) & is.null(save_parameters)) {
    return(comp)
  }
}
