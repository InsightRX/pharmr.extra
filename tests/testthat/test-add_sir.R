## Helpers ------------------------------------------------------------------

make_model_with_cov <- function() {
  pharmr::read_model_from_string(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n$ERROR\nY=F+EPS(1)\n$THETA (0,10) ; POP_CL\n$THETA (0,50) ; POP_V\n$SIGMA 0.1\n$EST METHOD=1\n$COV UNCOND\n"
  )
}

make_model_without_cov <- function() {
  pharmr::read_model_from_string(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n$ERROR\nY=F+EPS(1)\n$THETA (0,10) ; POP_CL\n$THETA (0,50) ; POP_V\n$SIGMA 0.1\n$EST METHOD=1\n"
  )
}

## add_sir() ----------------------------------------------------------------

test_that("add_sir() adds SIRSAMPLE and SIRNITER to $COV record", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  mod_sir <- add_sir(mod, options = list(niter = 2, samples = 500))
  expect_s3_class(mod_sir, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("SIRSAMPLE=500", mod_sir$code))
  expect_true(grepl("SIRNITER=2", mod_sir$code))
})

test_that("add_sir() uses custom niter and samples values", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  mod_sir <- add_sir(mod, options = list(niter = 5, samples = 2000))
  expect_true(grepl("SIRSAMPLE=2000", mod_sir$code))
  expect_true(grepl("SIRNITER=5", mod_sir$code))
})

test_that("add_sir() returns model unchanged when niter <= 0", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()

  mod_no_sir <- add_sir(mod, options = list(niter = 0, samples = 1000))
  expect_false(grepl("SIRSAMPLE", mod_no_sir$code))
  expect_false(grepl("SIRNITER", mod_no_sir$code))

  mod_no_sir2 <- add_sir(mod, options = list(niter = -1, samples = 1000))
  expect_false(grepl("SIRSAMPLE", mod_no_sir2$code))
  expect_false(grepl("SIRNITER", mod_no_sir2$code))
})

test_that("add_sir() errors when model has no $COV record", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  expect_error(
    add_sir(mod, options = list(niter = 1, samples = 1000)),
    "\\$COVARIANCE"
  )
})

test_that("add_sir() errors when options is not a list", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  expect_error(
    add_sir(mod, options = TRUE),
    "`options` should be a `list`"
  )
  expect_error(
    add_sir(mod, options = "sir"),
    "`options` should be a `list`"
  )
})

test_that("add_sir() errors when required option keys are missing", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  expect_error(
    add_sir(mod, options = list(niter = 1)),
    "requires T/F or a list"
  )
  expect_error(
    add_sir(mod, options = list(samples = 1000)),
    "requires T/F or a list"
  )
})

## Integration via create_model() -------------------------------------------

test_that("create_model() does not add SIR by default (niter <= 0)", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)
  expect_false(grepl("SIRSAMPLE", mod$code))
  expect_false(grepl("SIRNITER", mod$code))
})

test_that("create_model() adds SIR when sir_options has niter > 0", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "iv",
    sir_options = list(niter = 1, samples = 1000),
    verbose = FALSE
  )
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("SIRSAMPLE=1000", mod$code))
  expect_true(grepl("SIRNITER=1", mod$code))
})

test_that("create_model() uses custom SIR option values", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "iv",
    sir_options = list(niter = 3, samples = 2000),
    verbose = FALSE
  )
  expect_true(grepl("SIRSAMPLE=2000", mod$code))
  expect_true(grepl("SIRNITER=3", mod$code))
})
