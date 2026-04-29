library(mockery)

## TODO: needs tests for main run_nlme function

test_that("run_nlme mu_reference='auto': applies mu-referencing when SAEM + not mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_false(pharmr::has_mu_reference(mod_saem))

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_message(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
    ),
    "mu-referenc"
  )
  expect_true(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference='auto': no message when SAEM already mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = TRUE)
  expect_true(pharmr::has_mu_reference(mod_saem))

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_message(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
    )
  )
})

test_that("run_nlme mu_reference=TRUE: always applies mu-referencing (FOCE model)", {
  local_pharmr.extra_options()
  mod_foce <- create_model(estimation_method = "foce", mu_reference = FALSE)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_message(
    tryCatch(
      run_nlme(mod_foce, id = "run1", path = withr::local_tempdir(),
               mu_reference = TRUE, verbose = FALSE),
      error = function(e) NULL
    ),
    "mu-referenc"
  )
  expect_true(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference=FALSE: warns when SAEM model is not mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_false(pharmr::has_mu_reference(mod_saem))

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_warning(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = FALSE, verbose = FALSE),
      error = function(e) NULL
    ),
    "not mu-referenced"
  )
  # Model should NOT have been mu-referenced
  expect_false(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference=FALSE: no warning when SAEM already mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = TRUE)

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_warning(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = FALSE, verbose = FALSE),
      error = function(e) NULL
    )
  )
})

test_that("run_nlme mu_reference='auto': no warning/message for FOCE model", {
  local_pharmr.extra_options()
  mod_foce <- create_model(estimation_method = "foce")

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_warning(
    tryCatch(
      run_nlme(mod_foce, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
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
    "No \\$DATA record found"
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

test_that("run_nlme / prepare_run_folder strip surrounding quotes from column names", {
  local_pharmr.extra_options()

  ## Data.frame with column names wrapped in literal quote characters —
  ## Pharmpy rejects these (not valid Python identifiers) and NONMEM cannot
  ## parse a CSV whose header row starts with a quote character.
  df <- data.frame(
    ID = rep(1:2, each = 3),
    TIME = rep(c(0, 1, 2), 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = rep(c(100, 0, 0), 2),
    CMT = 1,
    EVID = rep(c(1, 0, 0), 2),
    MDV = rep(c(1, 0, 0), 2)
  )
  names(df) <- paste0('"', names(df), '"')

  mod <- create_model(route = "iv", verbose = FALSE)
  td <- withr::local_tempdir()

  obj <- prepare_run_folder(
    id = "run1", model = mod, path = td, data = df,
    auto_stack_encounters = FALSE, verbose = FALSE
  )

  first_line <- readLines(file.path(obj$fit_folder, "data.csv"), n = 1)
  expect_false(grepl('^"', first_line))
  expect_equal(
    strsplit(first_line, ",", fixed = TRUE)[[1]],
    c("ID", "TIME", "DV", "AMT", "CMT", "EVID", "MDV")
  )

  ## Also covers the filename path: when the source CSV has quoted headers,
  ## the written dataset should have them stripped.
  quoted_csv <- file.path(withr::local_tempdir(), "quoted.csv")
  writeLines(c('"ID","TIME","DV"', "1,0,0", "1,1,10"), quoted_csv)

  obj2 <- prepare_run_folder(
    id = "run1", model = mod, path = withr::local_tempdir(), data = quoted_csv,
    auto_stack_encounters = FALSE, verbose = FALSE
  )
  first_line2 <- readLines(file.path(obj2$fit_folder, "data.csv"), n = 1)
  expect_false(grepl('^"', first_line2))
  expect_equal(
    strsplit(first_line2, ",", fixed = TRUE)[[1]],
    c("ID", "TIME", "DV")
  )
})

test_that("unquote_column_names strips a single pair of surrounding quotes", {
  df <- data.frame(a = 1, b = 2, c = 3)
  names(df) <- c('"a"', "'b'", "c")
  out <- unquote_column_names(df)
  expect_equal(names(out), c("a", "b", "c"))

  ## Unmatched or internal quotes left alone
  df2 <- data.frame(x = 1, y = 2, z = 3)
  names(df2) <- c('"x', 'y"', 'a"b')
  out2 <- unquote_column_names(df2)
  expect_equal(names(out2), c('"x', 'y"', 'a"b'))

  ## NULL and non-data.frame input returned unchanged
  expect_null(unquote_column_names(NULL))
  expect_equal(unquote_column_names("not a df"), "not a df")
})

test_that("run_nlme converts data.frame input to a CSV file path", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, data = dat, id = "run1", path = withr::local_tempdir(),
             verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(is.character(captured_data))
  expect_match(captured_data, "\\.csv$")
  expect_true(file.exists(captured_data))

  ## Round-trip: file should contain the same data we passed in
  written <- read.csv(captured_data)
  expect_equal(written, dat, ignore_attr = TRUE)
})

test_that("run_nlme passes through a CSV file path unchanged", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)
  csv_path <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(ID = 1, TIME = 0, DV = 0, AMT = 100, CMT = 1, EVID = 1, MDV = 1),
    csv_path, quote = FALSE, row.names = FALSE
  )

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, data = csv_path, id = "run1", path = withr::local_tempdir(),
             verbose = FALSE),
    error = function(e) NULL
  )

  expect_identical(captured_data, csv_path)
})

test_that("run_nlme accepts NULL data (uses dataset embedded in model)", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(), verbose = FALSE),
    error = function(e) NULL
  )

  expect_null(captured_data)
})

test_that("run_nlme aborts when `data` is neither data.frame, character, nor NULL", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)

  expect_error(
    run_nlme(mod, data = list(a = 1), id = "run1",
             path = withr::local_tempdir(), verbose = FALSE),
    "unknown type"
  )
})

test_that("change_nonmem_dataset preserves whitespace and formatting", {
  # Extra whitespace around the path is preserved verbatim
  model_code <- "$PROB TEST\n$DATA    old_data.csv    IGNORE=@   \n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result, "\\$DATA    new_data\\.csv    IGNORE=@   ")

  # Tabs are preserved verbatim
  model_code_tabs <- "$PROB TEST\n$DATA\told_data.csv\tIGNORE=@\n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code_tabs, "new_data.csv")
  expect_match(result, "\\$DATA\tnew_data\\.csv\tIGNORE=@")
})

