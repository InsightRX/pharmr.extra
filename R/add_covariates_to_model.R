#' Wrapper function to add covariates to a pharmpy model
#'
#' @inheritParams create_model
#' @param model TODO
#'
#' @returns TODO
#'
#' @export
add_covariates_to_model <- function(
  model,
  covariates,
  data = NULL
) {
  dataset <- load_data_wrapper(data)
  ## pharmpy's add_covariate_effect() reads `model.dataset` to compute the
  ## covariate centering value (mean/median). When called from create_model()
  ## the dataset has not yet been attached, so attach it here before adding
  ## any covariate effects, then unload again so that create_model()'s later
  ## set_dataset() + clean_modelfit_data() flow sees the same fresh state it
  ## would otherwise see (avoids leaving a half-cleaned datainfo behind that
  ## breaks the subsequent set_dataset() on datasets with non-numeric columns).
  attached_temporarily <- FALSE
  if(is.null(model$dataset) && !is.null(dataset)) {
    model <- model |>
      pharmr::unload_dataset() |>
      pharmr::set_dataset(path_or_df = dataset, datatype = "nonmem")
    attached_temporarily <- TRUE
  }
  allowed_effects <- c("lin", "pow", "exp", "piece_lin", "cat", "cat2")
  for(par in names(covariates)) {
    for(covt in names(covariates[[par]])) {
      effect <- covariates[[par]][[covt]]
      if(!is.null(dataset) && !covt %in% names(dataset)) {
        warning("Covariate `", covt, "` not found in data, skipping.")
      } else {
        if(!effect %in% allowed_effects) {
          warning("Requested covariate effect type `", effect, "` not recognized, skipping.")
        } else {
          model <- pharmr::add_covariate_effect(
            model = model,
            parameter = par,
            covariate = covt,
            effect = effect
          )
        }
      }
    }
  }
  if(attached_temporarily) {
    model <- pharmr::unload_dataset(model)
  }
  model
}
