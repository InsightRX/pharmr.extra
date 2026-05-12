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


test_that("data as CSV filename: covariate present in file is added", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv(dat, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  covs <- list(CL = list(WT = "lin"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_no_warning(
    out <- add_covariates_to_model(model = mod, covariates = covs, data = tmp)
  )
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("WT", out$statements$before_odes$full_expression("CL")))
})


test_that("data as CSV filename: missing covariate warns and is skipped", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
    # WT intentionally absent
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv(dat, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  covs <- list(CL = list(WT = "lin"))
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_warning(
    out <- add_covariates_to_model(model = mod, covariates = covs, data = tmp),
    "Covariate `WT` not found in data, skipping."
  )
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("WT", out$statements$before_odes$full_expression("CL")))
})


## Regression test: pharmpy's add_covariate_effect() needs model.dataset to
## compute the covariate centering value. Previously, calling create_model()
## with both `data` and `covariates` failed with
## "AttributeError: 'NoneType' object has no attribute 'groupby'" because the
## dataset was attached only after add_covariates_to_model() ran.
test_that("create_model() with `covariates` works (model dataset not yet attached)", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = rep(1:3, each = 4),
    TIME = rep(c(0, 1, 2, 4), 3),
    DV = c(0, 10, 8, 5, 0, 12, 9, 6, 0, 9, 7, 4),
    AMT = rep(c(100, 0, 0, 0), 3),
    CMT = 1,
    EVID = rep(c(1, 0, 0, 0), 3),
    MDV = rep(c(1, 0, 0, 0), 3),
    WT = rep(c(70, 80, 65), each = 4),
    CRCL = rep(c(80, 100, 90), each = 4)
  )
  mod <- create_model(
    route = "oral",
    data = dat,
    covariates = list(CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow")),
    verbose = FALSE
  )
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("WT", mod$statements$before_odes$full_expression("CL")))
  expect_true(grepl("CRCL", mod$statements$before_odes$full_expression("CL")))
  expect_true(grepl("WT", mod$statements$before_odes$full_expression("V")))
})


## Regression test: when create_model() is called with a dataset containing
## non-numeric columns (e.g. a character GROUP column), the temporary dataset
## attached inside add_covariates_to_model() must not leave datainfo state that
## breaks the subsequent set_dataset() + clean_modelfit_data() flow.
test_that("create_model() with `covariates` works when data has character cols", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = rep(1:3, each = 4),
    TIME = rep(c(0, 1, 2, 4), 3),
    DV = c(0, 10, 8, 5, 0, 12, 9, 6, 0, 9, 7, 4),
    AMT = rep(c(100, 0, 0, 0), 3),
    CMT = 1,
    EVID = rep(c(1, 0, 0, 0), 3),
    MDV = rep(c(1, 0, 0, 0), 3),
    WT = rep(c(70, 80, 65), each = 4),
    GROUP = rep(c("DrugA", "DrugB", "DrugA"), each = 4)
  )
  mod <- create_model(
    route = "oral",
    data = dat,
    covariates = list(CL = list(WT = "pow")),
    verbose = FALSE
  )
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("WT", mod$statements$before_odes$full_expression("CL")))
})
