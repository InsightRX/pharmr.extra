library(mockery)

## TODO: needs tests for main run_nlme function

test_that("run_nlme warns when SAEM model is not mu-referenced", {
  mod_saem <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_warning(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir()),
      error = function(e) NULL
    ),
    "not mu-referenced"
  )
})

test_that("run_nlme does not warn when SAEM model is mu-referenced", {
  mod_saem <- create_model(estimation_method = "saem", mu_reference = TRUE)
  expect_no_warning(
    expect_warning(
      tryCatch(
        run_nlme(mod_saem, id = "run1", path = withr::local_tempdir()),
        error = function(e) NULL
      ),
      "No parameter estimates"
    )
  )
})

test_that("run_nlme does not warn when FOCE model is not mu-referenced", {
  mod_foce <- create_model(estimation_method = "foce")
  expect_no_warning(
    expect_warning(
      tryCatch(
        run_nlme(mod_foce, id = "run1", path = withr::local_tempdir()),
        error = function(e) NULL
      ),
      "No parameter estimates"
    )
  )
})

test_that("get_new_run_number works correctly", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Test 1: Empty directory should return 1
  expect_equal(get_new_run_number(temp_dir), 1)
  
  # Test 2: With existing run folders
  dir.create(file.path(temp_dir, "run1"))
  dir.create(file.path(temp_dir, "run2"))
  expect_equal(get_new_run_number(temp_dir), 3)
  
  # Test 3: Non-sequential numbers
  unlink(file.path(temp_dir, "run2"))
  dir.create(file.path(temp_dir, "run5"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 4: With non-run folders present
  dir.create(file.path(temp_dir, "other_folder"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 5: With invalid run folder names
  dir.create(file.path(temp_dir, "runA"))
  dir.create(file.path(temp_dir, "run"))
  expect_equal(get_new_run_number(temp_dir), 6)
})

test_that("change_nonmem_dataset handles different input formats correctly", {
  # Test single-line string input
  model_code_single <- "$PROB TEST\n$DATA old_data.csv IGNORE=@\n$INPUT ID TIME DV"
  result1 <- change_nonmem_dataset(model_code_single, "new_data.csv")
  expect_match(result1, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test vector input
  model_code_vector <- c("$PROB TEST", "$DATA old_data.csv IGNORE=@", "$INPUT ID TIME DV")
  result2 <- change_nonmem_dataset(model_code_vector, "new_data.csv")
  expect_match(result2, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test with multiple options after dataset
  model_code <- "$PROB TEST\n$DATA old_data.csv IGNORE=@ ACCEPT=(DV.GT.0)\n$INPUT ID TIME DV"
  result3 <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result3, "\\$DATA new_data\\.csv IGNORE=@ ACCEPT=\\(DV\\.GT\\.0\\)")
})

test_that("change_nonmem_dataset handles errors appropriately", {
  # Test missing $DATA line
  model_code_no_data <- "$PROB TEST\n$INPUT ID TIME DV"
  expect_error(
    change_nonmem_dataset(model_code_no_data, "new_data.csv"),
    "No \\$DATA line found in the model file"
  )
})

test_that("run_nlme removes tables from model when remove_tables = TRUE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)
  expect_true(length(get_tables_in_model_code(mod$code)) > 0)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             remove_tables = TRUE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(is.null(captured_model))
  expect_length(get_tables_in_model_code(captured_model$code), 0)
})

test_that("run_nlme preserves tables when remove_tables = FALSE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)
  n_tables <- length(get_tables_in_model_code(mod$code))
  expect_true(n_tables > 0)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             remove_tables = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(is.null(captured_model))
  expect_length(get_tables_in_model_code(captured_model$code), n_tables)
})

test_that("change_nonmem_dataset preserves whitespace and formatting", {
  # Test with extra whitespace
  model_code <- "$PROB TEST\n$DATA    old_data.csv    IGNORE=@   \n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test with tabs
  model_code_tabs <- "$PROB TEST\n$DATA\told_data.csv\tIGNORE=@\n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code_tabs, "new_data.csv")
  expect_match(result, "\\$DATA new_data\\.csv IGNORE=@")
})

