test_that("updates estimation method for all valid methods", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  # Test all allowed methods:
  allowed_methods <- c("FO", "FOCE", "ITS", "IMPMAP", "IMP", "SAEM")
  
  for (method in allowed_methods) {
    updated_mod <- update_estimation_method(mod, method, verbose = FALSE)
    steps <- updated_mod$execution_steps$to_dataframe()
    expect_true(
      tolower(method) %in% tolower(steps$method),
      info = paste("Failed for method:", method)
    )
    expect_s3_class(updated_mod, "pharmpy.model.external.nonmem.model.Model")
  }
})

test_that("input is not case sensitive", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  # lowercase:
  updated_mod <- update_estimation_method(mod, "foce", verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))
  
  # mixed case:
  updated_mod <- update_estimation_method(mod, "FoCe", verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))
})

test_that("updates estimation method when model already has one", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(
    route = "iv", 
    data = dat, 
    estimation_method = "foce",
    verbose = FALSE
  )
  
  # Verify initial method:
  initial_steps <- mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(initial_steps$method))
  
  # Update to different method:
  updated_mod <- update_estimation_method(mod, "SAEM", verbose = FALSE)
  updated_steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("saem" %in% tolower(updated_steps$method))
})

test_that("works with different route types", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  # IV model:
  mod_iv <- create_model(route = "iv", data = dat, verbose = FALSE)
  updated_iv <- update_estimation_method(mod_iv, "ITS", verbose = FALSE)
  steps_iv <- updated_iv$execution_steps$to_dataframe()
  expect_true("its" %in% tolower(steps_iv$method))
  
  # oral model:
  mod_oral <- create_model(route = "oral", data = dat, verbose = FALSE)
  updated_oral <- update_estimation_method(mod_oral, "ITS", verbose = FALSE)
  steps_oral <- updated_oral$execution_steps$to_dataframe()
  expect_true("its" %in% tolower(steps_oral$method))
})

test_that("warns and uses first method when multiple methods provided", {
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  # Should warn when multiple methods provided:
  expect_warning(
    {
      updated_mod <- update_estimation_method(
        mod, c("FOCE", "SAEM"), verbose = FALSE
      )
    },
    "supports only a single estimation method"
  )
  
  # Should use first method:
  steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))
  expect_false("saem" %in% tolower(steps$method))
})

test_that("errors on invalid estimation method", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  expect_error(
    update_estimation_method(mod, "INVALID", verbose = FALSE),
    "The requested estimation method was not recognized"
  )
  
  expect_error(
    update_estimation_method(mod, "BOGUS", verbose = FALSE),
    "The requested estimation method was not recognized"
  )
})
