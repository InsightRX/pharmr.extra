test_that("create_model_from_file errors when model_file is not a string", {
  expect_error(
    create_model_from_file(model_file = 123, data = data.frame()),
    "Model file should be a string"
  )
  expect_error(
    create_model_from_file(model_file = NULL, data = data.frame()),
    "Model file should be a string"
  )
  expect_error(
    create_model_from_file(model_file = list("a"), data = data.frame()),
    "Model file should be a string"
  )
})

test_that("create_model_from_file errors when model file does not exist", {
  expect_error(
    create_model_from_file(
      model_file = "nonexistent_model.mod",
      data = data.frame()
    ),
    "does not exist"
  )
})

test_that("create_model_from_file reads model file and calls pharmr", {
  # Create a temporary model file
  model_code <- c(
    "$PROBLEM Test",
    "$INPUT ID TIME DV",
    "$DATA test.csv IGNORE=@",
    "$PRED Y = THETA(1) + EPS(1)",
    "$THETA 1",
    "$SIGMA 1",
    "$EST METHOD=1"
  )
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  test_data <- data.frame(ID = 1, TIME = 0, DV = 1)
  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  mock_model_with_data <- structure(
    list(has_data = TRUE),
    class = "pharmpy.model.model.Model"
  )

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    mock_model
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    mock_model_with_data
  )

  result <- create_model_from_file(model_file = tmp_mod, data = test_data)
  expect_s3_class(result, "pharmpy.model.model.Model")

  unlink(tmp_mod)
})

test_that("create_model_from_file reads data from CSV file path", {
  # Create a temporary model file
  model_code <- c(
    "$PROBLEM Test",
    "$INPUT ID TIME DV",
    "$DATA test.csv IGNORE=@",
    "$PRED Y = THETA(1) + EPS(1)",
    "$THETA 1",
    "$SIGMA 1"
  )
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  # Create a temporary data file
  test_data <- data.frame(ID = 1, TIME = c(0, 1), DV = c(0, 5))
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, tmp_csv, row.names = FALSE)

  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  mock_model_with_data <- structure(
    list(has_data = TRUE),
    class = "pharmpy.model.model.Model"
  )

  read_model_mock <- mockery::mock(mock_model)
  set_dataset_mock <- mockery::mock(mock_model_with_data)

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    read_model_mock
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    set_dataset_mock
  )

  result <- create_model_from_file(model_file = tmp_mod, data = tmp_csv)
  expect_s3_class(result, "pharmpy.model.model.Model")

  # Verify set_dataset was called with a data.frame (read from CSV)
  set_dataset_args <- mockery::mock_args(set_dataset_mock)[[1]]
  expect_s3_class(set_dataset_args[[2]], "data.frame")

  unlink(c(tmp_mod, tmp_csv))
})

test_that("create_model_from_file accepts data.frame directly", {
  model_code <- c("$PROBLEM Test", "$PRED Y = THETA(1)", "$THETA 1")
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  test_data <- data.frame(ID = 1, TIME = 0, DV = 1)
  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  mock_model_with_data <- structure(
    list(has_data = TRUE),
    class = "pharmpy.model.model.Model"
  )

  set_dataset_mock <- mockery::mock(mock_model_with_data)

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    mock_model
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    set_dataset_mock
  )

  result <- create_model_from_file(model_file = tmp_mod, data = test_data)
  expect_s3_class(result, "pharmpy.model.model.Model")

  # Verify set_dataset was called with the original data.frame
  set_dataset_args <- mockery::mock_args(set_dataset_mock)[[1]]
  expect_true(inherits(set_dataset_args[[2]], "data.frame"))
  expect_equal(nrow(set_dataset_args[[2]]), 1)

  unlink(tmp_mod)
})

test_that("create_model_from_file passes model code as single string to pharmr", {
  model_code <- c(
    "$PROBLEM Test",
    "$INPUT ID TIME DV",
    "$PRED Y = THETA(1) + EPS(1)",
    "$THETA 1",
    "$SIGMA 1"
  )
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  test_data <- data.frame(ID = 1, TIME = 0, DV = 1)
  mock_model <- structure(list(), class = "pharmpy.model.model.Model")

  read_model_mock <- mockery::mock(mock_model)

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    read_model_mock
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    mock_model
  )

  create_model_from_file(model_file = tmp_mod, data = test_data)

  # Verify read_model_from_string was called with lines collapsed into one string
  read_model_args <- mockery::mock_args(read_model_mock)[[1]]
  expect_true(grepl("\\$PROBLEM Test\n\\$INPUT", read_model_args[[1]]))

  unlink(tmp_mod)
})

test_that("create_model_from_file returns model without data when data is NULL", {
  model_code <- c("$PROBLEM Test", "$PRED Y = THETA(1)", "$THETA 1")
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  set_dataset_mock <- mockery::mock(mock_model)

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    mock_model
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    set_dataset_mock
  )

  result <- create_model_from_file(model_file = tmp_mod, data = NULL)

  # set_dataset should not have been called
  expect_equal(mockery::mock_calls(set_dataset_mock), list())
  # model should still be returned
  expect_s3_class(result, "pharmpy.model.model.Model")

  unlink(tmp_mod)
})

# Tests using real fixture files (run.mod + run.ext from a completed NONMEM run)
# Final estimates in run.ext: THETA1=1.32434, THETA2=27.9381, THETA3=181.119
# Original inits in run.mod:  THETA1=0.5,     THETA2=6.52,    THETA3=116.0

test_that("create_model_from_file reads model without ext_file using original inits", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")

  result <- create_model_from_file(model_file = mod_file)

  params <- result$parameters$to_dataframe()
  expect_s3_class(result, "pharmpy.model.external.nonmem.model.Model")
  expect_equal(params["POP_KA", "value"], 0.5,   tolerance = 1e-4)
  expect_equal(params["POP_CL", "value"], 6.52,  tolerance = 1e-4)
  expect_equal(params["POP_V",  "value"], 116.0, tolerance = 1e-4)
})

test_that("create_model_from_file updates initial estimates from ext_file", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")
  ext_file <- testthat::test_path("fixtures", "run_with_ext", "run.ext")

  result <- create_model_from_file(model_file = mod_file, ext_file = ext_file)

  params <- result$parameters$to_dataframe()
  expect_s3_class(result, "pharmpy.model.external.nonmem.model.Model")
  expect_equal(params["POP_KA", "value"], 1.32434, tolerance = 1e-4)
  expect_equal(params["POP_CL", "value"], 27.9381, tolerance = 1e-4)
  expect_equal(params["POP_V",  "value"], 181.119, tolerance = 1e-3)
})

test_that("create_model_from_file ext_file produces different inits than no ext_file", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")
  ext_file <- testthat::test_path("fixtures", "run_with_ext", "run.ext")

  result_base    <- create_model_from_file(model_file = mod_file)
  result_updated <- create_model_from_file(model_file = mod_file, ext_file = ext_file)

  params_base    <- result_base$parameters$to_dataframe()
  params_updated <- result_updated$parameters$to_dataframe()

  expect_false(
    isTRUE(all.equal(params_base$value, params_updated$value, tolerance = 1e-4))
  )
})

test_that("create_model_from_file errors when ext_file does not exist", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")

  expect_error(
    create_model_from_file(model_file = mod_file, ext_file = "nonexistent.ext"),
    "does not exist"
  )
})

test_that("create_model_from_file works without data argument (default NULL)", {
  model_code <- c("$PROBLEM Test", "$PRED Y = THETA(1)", "$THETA 1")
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  set_dataset_mock <- mockery::mock(mock_model)

  mockery::stub(
    create_model_from_file,
    "pharmr::read_model_from_string",
    mock_model
  )
  mockery::stub(
    create_model_from_file,
    "pharmr::set_dataset",
    set_dataset_mock
  )

  # Call without data argument — should use default NULL
  result <- create_model_from_file(model_file = tmp_mod)

  expect_s3_class(result, "pharmpy.model.model.Model")
  expect_equal(mockery::mock_calls(set_dataset_mock), list())

  unlink(tmp_mod)
})

test_that("create_model_from_file circumvents bug in pharmpy with dummy_eta and can add peripheral comparment", {
  model <- create_model_from_file(test_path("fixtures", "model_with_dummyeta", "run1.mod"))
  mod2 <- model |>
    pharmr::add_peripheral_compartment()
  expect_s3_class(mod2, "pharmpy.model.model.Model")
  expect_true(all(c("POP_QP1", "POP_VP1") %in% mod2$parameters$names))
})
