# TODO: Could not test this due to error: "Error in
# `get_nmfe_location()`: ! Pharmpy is not configured to run
# NONMEM." Probably means we need to skip tests if NONMEM isn't installed. Should
# write a skip_no_nonmem() helper.

test_that("create_vpc_data() requires either model or fit object", {
  local_pharmr.extra_options()
  expect_error(
    create_vpc_data(fit = NULL, model = NULL),
    "Either a `fit` object with a model attached, or a `model` argument is required"
  )

  fit_no_model <- list(parameter_estimates = c(CL = 5, V = 50))
  expect_error(
    create_vpc_data(fit = fit_no_model, model = NULL),
    "Either a `fit` object with a model attached, or a `model` argument is required"
  )
})

test_that("create_vpc_data works with model and parameters", {
  skip()
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3, 4),
    DV = c(0, 10, 5, 3, 2),
    AMT = c(100, 0, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0, 0),
    MDV = c(1, 0, 0, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  pars <- list(POP_CL = 5, POP_V = 10)
  # FIXME: Error in `get_nmfe_location()`:
  # ! Pharmpy is not configured to run NONMEM.
  out <- create_vpc_data(model = mod, parameters = pars, n = 2, verbose = FALSE)
  
  # Check output structure
  # expect_type(result, "list")
  # expect_named(result, c("obs", "sim"))
  # expect_s3_class(result$obs, "data.frame")
  # expect_s3_class(result$sim, "data.frame")
  # 
  # # Check that obs has required columns
  # expect_true("ID" %in% names(result$obs))
  # expect_true("TIME" %in% names(result$obs))
  # expect_true("DV" %in% names(result$obs))
  # expect_true("PRED" %in% names(result$obs))
  # expect_true("EVID" %in% names(result$obs))
  # expect_true("MDV" %in% names(result$obs))
  # 
  # # Check that sim has required columns
  # expect_true("ID" %in% names(result$sim))
  # expect_true("TIME" %in% names(result$sim))
  # expect_true("PRED" %in% names(result$sim))
  # 
  # # Check that sim length is a multiple of obs length
  # expect_equal(nrow(result$sim) %% nrow(result$obs), 0)
})
