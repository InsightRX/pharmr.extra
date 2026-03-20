#' Get covariate data (single row per subject) from a dataset given model
#' 
#' @param model Pharmpy NONMEM model object
#' 
#' @export
#' 
#' @returns a data.frame or tibble with covariates for model
#' 
get_covariate_data <- function(model) {
  mod_info <- get_model_info(model)
  cov_names <- mod_info$covariates
  model$dataset |>
    as.data.frame() |>
    dplyr::filter(! duplicated(.data$ID)) |>
    dplyr::select(c("ID", cov_names))
}
