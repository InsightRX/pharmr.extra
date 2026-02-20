test_that("returns pharmpy model unchanged when already a model", {
  local_pharmr.extra_options()
  mod <- create_model(verbose = FALSE)
  out <- as_pharmpy_model(mod)
  expect_identical(out, mod)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("reads from export_pharmpy_model() RDS file", {
  local_pharmr.extra_options()
  mod <- create_model(verbose = FALSE)
  tmp <- withr::local_tempfile(fileext = ".rds")
  export_pharmpy_model(mod, tmp)
  out <- as_pharmpy_model(tmp)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("reads from NONMEM file path (.mod)", {
  local_pharmr.extra_options()
  code <- "$PROBLEM Test\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  tmp <- withr::local_tempfile(fileext = ".mod")
  writeLines(code, tmp)
  out <- as_pharmpy_model(tmp)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("reads from NONMEM file path (.ctl)", {
  local_pharmr.extra_options()
  code <- "$PROBLEM Test\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  tmp <- withr::local_tempfile(fileext = ".ctl")
  writeLines(code, tmp)
  out <- as_pharmpy_model(tmp)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("reads from NONMEM file path (.nmctl)", {
  local_pharmr.extra_options()
  code <- "$PROBLEM Test\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  tmp <- withr::local_tempfile(fileext = ".nmctl")
  writeLines(code, tmp)
  out <- as_pharmpy_model(tmp)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("reads from model code string", {
  local_pharmr.extra_options()
  code <- "$PROBLEM Test\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  out <- as_pharmpy_model(code)
  expect_s3_class(out, "pharmpy.model.model.Model")
})

test_that("errors when input is not model object or readable character", {
  expect_error(
    as_pharmpy_model(123),
    "Could not read model into Pharmpy"
  )
  expect_error(
    as_pharmpy_model(list(a = 1)),
    "Could not read model into Pharmpy"
  )
  expect_error(
    as_pharmpy_model(NULL),
    "Could not read model into Pharmpy"
  )
})
