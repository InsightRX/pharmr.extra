test_that("validates and returns valid NONMEM pharmpy model object", {
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
  out <- validate_model(mod)
  
  expect_equal(out, mod)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("handles single character string as model code", {
  local_pharmr.extra_options()
  # Single string (not a file path)
  mod <- "$PROBLEM Test\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  out <- validate_model(mod)

  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("errors when pharmpy model object is not NONMEM", {
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
  mod <- create_model(route = "iv", data = dat, tool = "nlmixr2", verbose = FALSE)
  
  expect_error(validate_model(mod), "Currently only NONMEM is supported")
})

test_that("errors when input is neither model object nor character", {
  expect_error(
    validate_model(123),
    "`model` should either be model code or a pharmpy model object"
  )
  
  expect_error(
    validate_model(list(a = 1)),
    "`model` should either be model code or a pharmpy model object"
  )
  
  expect_error(
    validate_model(NULL),
    "`model` should either be model code or a pharmpy model object"
  )
})
