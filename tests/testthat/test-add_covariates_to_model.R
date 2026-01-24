test_that("adding single covariate works", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80
  )
  covs <- list(CL = list(WT = "lin"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  out <- add_covariates_to_model(model = mod, covariates = covs, data = dat)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", mod$statements$before_odes$full_expression("CL")))
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
})

test_that("adding multiple covariates works", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80
  )
  covs <- list(CL = list(WT = "pow", CRCL = "lin"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  out <- add_covariates_to_model(model = mod, covariates = covs, data = dat)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", mod$statements$before_odes$full_expression("CL")))
  expect_false(grepl("CRCL", mod$statements$before_odes$full_expression("CL")))
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("CRCL", out$statements$before_odes$full_expression("CL")))
})



test_that("adding covariates to multiple parameters works", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80
  )
  covs <- list(
    CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow", CRCL = "lin")
  )
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  out <- add_covariates_to_model(model = mod, covariates = covs, data = dat)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", mod$statements$before_odes$full_expression("CL")))
  expect_false(grepl("CRCL", mod$statements$before_odes$full_expression("CL")))
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("CRCL", out$statements$before_odes$full_expression("CL")))
  expect_false(grepl("WT", mod$statements$before_odes$full_expression("V")))
  expect_false(grepl("CRCL", mod$statements$before_odes$full_expression("V")))
  expect_true(grepl("WT", out$statements$before_odes$full_expression("V")))
  expect_true(grepl("CRCL", out$statements$before_odes$full_expression("V")))
})


test_that("all effect types work", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80,
    AGE = 45,
    SEX = 1,
    RACE = 1
  )
  covs <- list(
    CL = list(
      WT = "lin",
      CRCL = "pow",
      AGE = "exp",
      SEX = "cat",
      RACE = "cat2"
    )
  )
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_no_warning(
    out <- add_covariates_to_model(model = mod, covariates = covs, data = dat)
  )
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("CRCL", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("AGE", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("SEX", out$statements$before_odes$full_expression("CL")))
  expect_true(grepl("RACE", out$statements$before_odes$full_expression("CL")))
})


test_that("warn and skip when covariate missing in data", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    CRCL = 80
  )
  covs <- list(CL = list(WT = "lin"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_warning(
    out <- add_covariates_to_model(model = mod, covariates = covs, data = dat),
    "Covariate `WT` not found in data, skipping."
  )
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", out$statements$before_odes$full_expression("CL")))
})


test_that("warn and skip when effect type is not valid", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80
  )
  covs <- list(CL = list(WT = "invalid_effect"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_warning(
    out <- add_covariates_to_model(model = mod, covariates = covs, data = dat),
    "Requested covariate effect type `invalid_effect` not recognized, skipping."
  )
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", out$statements$before_odes$full_expression("CL")))
})
