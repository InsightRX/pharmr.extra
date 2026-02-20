#' Get model info
#'
#' @description
#' The `get_*` family of functions are designed to get different types of model
#' information from a Pharmpy model.
#' 
#' @inheritParams as_pharmpy_model
#'
#' @returns For `get_model_info()`: a named list providing a structured summary
#'   of the model. For other `get_*` functions: a character string.
#' @export
get_model_info <- function(model) {
  # TODO: add safeguards for empty outputs in list? (i.e., no character(0)s)
  model <- as_pharmpy_model(model)
  list(
    id = model$name,
    description = model$description,
    advan = get_advan(model),
    code = pharmr::get_model_code(model),
    covariates = to_chr_vector(pharmr::get_model_covariates(model)),
    parameters = list(
      pk_parameter_names = pharmr::get_pk_parameters(model),
      central_clearance = to_chr_vector(pharmr::get_central_volume_and_clearance(model))[2],
      central_volume = to_chr_vector(pharmr::get_central_volume_and_clearance(model))[1],
      theta = pharmr::get_thetas(model)$inits,
      omega = pharmr::get_omegas(model)$inits,
      sigma = pharmr::get_sigmas(model)$inits
    ),
    compartments = list(
      compartment_names = model$statements$ode_system$compartment_names,
      obs_compartment = get_obs_compartment(model),
      n_transit_compartments = pharmr::get_number_of_transit_compartments(model),
      n_peripheral_compartments = pharmr::get_number_of_peripheral_compartments(model),
      lag_times = as.list(to_chr_vector(pharmr::get_lag_times(model))),
      bioavailability = as.list(to_chr_vector(pharmr::get_bioavailability(model)))
    ),
    error_model = get_error_model(model),
    absorption = get_absorption(model),
    elimination = get_elimination(model),
    linearity = get_ode_linearity(model),
    ltbs = is_ltbs_model(model)
  )
}

#' @rdname get_model_info
#' @export
get_error_model <- function(model) {
  dplyr::case_when(
    pharmr::has_additive_error_model(model) ~ "additive",
    pharmr::has_proportional_error_model(model) ~ "proportional",
    pharmr::has_combined_error_model(model) ~ "combined",
    TRUE ~ "other"
  )
}

#' @rdname get_model_info
#' @export
get_elimination <- function(model) {
  dplyr::case_when(
    pharmr::has_first_order_elimination(model) ~ "first_order",
    pharmr::has_zero_order_elimination(model) ~ "zero_order",
    pharmr::has_michaelis_menten_elimination(model) ~ "michaelis_menten",
    pharmr::has_mixed_mm_fo_elimination(model) ~ "mixed_mm_fo",
    TRUE ~ "other"
  )
}

#' @rdname get_model_info
#' @export
get_absorption <- function(model) {
  # TODO: Think about how to handle multi-formulation models, which may
  # use different methods for different formulations.
  dplyr::case_when(
    pharmr::has_instantaneous_absorption(model) ~ "instantaneous",
    pharmr::has_first_order_absorption(model) ~ "first_order",
    pharmr::has_seq_zo_fo_absorption(model) ~ "sequential_zero_first_order",
    pharmr::has_zero_order_absorption(model) ~ "zero_order",
    pharmr::has_weibull_absorption(model) ~ "weibull",
    TRUE ~ "other"
  )
}

#' @rdname get_model_info
#' @export
get_ode_linearity <- function(model) {
  dplyr::if_else(pharmr::has_linear_odes(model), "linear", "nonlinear")
}

# Some pharmr outputs are lists of symbols, which require a special approach
# to turn into a character vector:
to_chr_vector <- function(x) {
  vapply(x, as.character, character(1))
}
