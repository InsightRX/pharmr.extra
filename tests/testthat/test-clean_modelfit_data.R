# TODO: Because create_model() also calls clean_modelfit_data(), the tests below
# all need to use the `data` argument to ensure the original data set is being
# tested, rather than an already-once-cleaned data set from create_model(). We
# should think about this behaviour, because it will create unexpected results
# if a user calls create_model() then clean_modelfit_data(), particularly for
# LTBS models with LNDV columns.

test_that("works with basic model and data set", {
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
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)
  
  expect_equal(names(out$dataset), names(dat))
  expect_equal(out$dataset, dat, ignore_attr = TRUE)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("converts character columns to numeric when try_make_numeric = TRUE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = c("70", "70", "70")
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, try_make_numeric = TRUE, verbose = FALSE)
  expect_true(is.numeric(out$dataset$WT))
  expect_equal(out$dataset$WT, c(70, 70, 70))
})

test_that("removes character columns when try_make_numeric = FALSE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = c("70", "70", "70")
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, try_make_numeric = FALSE, verbose = FALSE)
  
  expect_false("WT" %in% names(out$dataset))
})

test_that("handles BLQ data with < in DV column", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3),
    DV = c(0, 10, "<3", 5),
    AMT = c(100, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)

  # Check that LLOQ column was added:
  expect_true("LLOQ" %in% names(out$dataset))
  # Check that DV values are correct (BLQ should be 0, LLOQ should be 3):
  expect_equal(out$dataset$DV, c(0, 10, 0, 5))
  expect_equal(out$dataset$LLOQ, c(0, 0, 3, 0))
})

test_that("handles character DV with non-BLQ values", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c("0", "10", "5"),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)
  
  # DV should be converted to numeric:
  expect_true(is.numeric(out$dataset$DV))
  expect_equal(out$dataset$DV, c(0, 10, 5))
})

test_that("converts NAs to 0s", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, NA),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = c(70, NA, 80)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)

  # Check that NAs are converted to 0:
  expect_equal(out$dataset$DV, c(0, 10, 0))
  expect_equal(out$dataset$WT, c(70, 0, 80))
})

test_that("handles LTBS model with LNDV column", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3),
    DV = c(0, 10, 5, 8),
    LNDV = c(0, 2.3, 1.6, 2.1),
    AMT = c(100, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, ruv = "ltbs", verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)

  # Check that LNDV becomes DV and original DV becomes ODV
  expect_true("ODV" %in% names(out$dataset))
  expect_equal(out$dataset$DV, dat$LNDV)
  expect_equal(out$dataset$ODV, dat$DV)
})

test_that("handles LTBS model without LNDV column", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3),
    DV = c(0, 2.3, 1.6, 2.1),  # Already log-transformed
    AMT = c(100, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, ruv = "ltbs", verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)

  # DV should remain unchanged when no LNDV column exists:
  expect_equal(out$dataset$DV, dat$DV)
  expect_false("ODV" %in% names(out$dataset))
})

test_that("accepts data as separate argument", {
  local_pharmr.extra_options()
  dat1 <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  dat2 <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 20, 10),
    AMT = c(200, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat1, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat2, verbose = FALSE)

  # Should use dat2, not dat1:
  expect_equal(out$dataset$DV, dat2$DV)
  expect_equal(out$dataset$AMT, dat2$AMT)
})



test_that("handles multiple character columns", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = c("70", "70", "70"),
    AGE = c("30", "30", "30")
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, verbose = FALSE)

  # Both character columns should be converted
  expect_true(is.numeric(out$dataset$WT))
  expect_true(is.numeric(out$dataset$AGE))
  expect_equal(out$dataset$WT, c(70, 70, 70))
  expect_equal(out$dataset$AGE, c(30, 30, 30))
})

test_that("handles character columns that cannot be converted to numeric", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    CAT = c("A", "B", "C")  # Cannot be converted to numeric
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  out <- clean_modelfit_data(mod, data = dat, try_make_numeric = TRUE, verbose = FALSE)

  # Character column that can't be converted should become NA then 0:
  expect_true(is.numeric(out$dataset$CAT))
  expect_equal(out$dataset$CAT, c(0, 0, 0)) # All become 0 after NA conversion
})
