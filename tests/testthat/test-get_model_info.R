# get_model_info () -----------------------------------------------------------
test_that("returns named list with expected structure", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  out <- get_model_info(mod)
  expect_type(out, "list")
  expect_named(
    out,
    c(
      "id", "description", "advan", "code", "covariates", "parameters",
      "compartments", "error_model", "absorption", "elimination",
      "linearity", "ltbs", "estimation_steps"
    )
  )
  expect_named(
    out$parameters,
    c(
      "pk_parameter_names", "central_clearance", "central_volume",
      "theta", "omega", "sigma"
    )
  )
  expect_named(
    out$compartments,
    c(
      "compartment_names", "obs_compartment", "n_transit_compartments",
      "n_peripheral_compartments", "lag_times", "bioavailability"
    )
  )
})

test_that("gets model info from Pharmpy model objects", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", n_cmt = 1, verbose = FALSE)
  out <- get_model_info(mod)
  expect_type(out, "list")
  expect_named(
    out,
    c(
      "id", "description", "advan", "code", "covariates", "parameters",
      "compartments", "error_model", "absorption", "elimination",
      "linearity", "ltbs", "estimation_steps"
    )
  )
  expect_equal(out$advan, 1L)
  expect_equal(out$elimination, "first_order")
  expect_equal(out$absorption, "instantaneous")
  expect_equal(out$compartments$obs_compartment, 1L)
})

test_that("gets model info from .mod files", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  out <- get_model_info(mod_path)
  expect_equal(out$advan, 1L)
  expect_equal(out$error_model, "additive")
})

test_that("returns correct values for base_iv.mod", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  out <- get_model_info(mod)
  expect_equal(out$advan, 1L)
  expect_equal(out$error_model, "additive")
  expect_equal(out$elimination, "first_order")
  expect_equal(out$absorption, "instantaneous")
  expect_equal(out$linearity, "linear")
  expect_false(out$ltbs)
  expect_equal(out$compartments$obs_compartment, 1L)
  expect_equal(out$compartments$n_transit_compartments, 0L)
  expect_equal(out$compartments$n_peripheral_compartments, 0L)
})

test_that("parameters list has expected types for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  out <- get_model_info(mod)
  expect_vector(out$parameters$theta)
  expect_vector(out$parameters$omega)
  expect_vector(out$parameters$sigma)
  expect_type(out$parameters$central_clearance, "character")
  expect_type(out$parameters$central_volume, "character")
})

# get_estimation_steps () -----------------------------------------------------
test_that("returns list of estimation steps for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  out <- get_estimation_steps(mod)
  expect_type(out, "list")
  expect_length(out, 1L)
  expect_named(out[[1]], c("method", "interaction", "evaluation", "parameter_uncertainty_method", "laplace"))
  expect_equal(out[[1]]$method, "FOCE")
  expect_false(out[[1]]$interaction)
  expect_false(out[[1]]$evaluation)
  expect_equal(out[[1]]$parameter_uncertainty_method, "SANDWICH")
  expect_false(out[[1]]$laplace)
})

# get_error_model () ----------------------------------------------------------
test_that("returns expected string for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  expect_equal(get_error_model(mod), "additive")
})

# get_elimination () ----------------------------------------------------------
test_that("returns expected string for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  expect_equal(get_elimination(mod), "first_order")
})

# get_absorption () -----------------------------------------------------------
test_that("returns expected string for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  expect_equal(get_absorption(mod), "instantaneous")
})

# get_ode_linearity () --------------------------------------------------------
test_that("returns expected string for base_iv", {
  local_pharmr.extra_options()
  mod_path <- fs::path_package("pharmr.extra", "models/nonmem/base_iv.mod")
  mod <- as_pharmpy_model(mod_path)
  expect_equal(get_ode_linearity(mod), "linear")
})
