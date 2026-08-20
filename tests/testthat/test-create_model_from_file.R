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

test_that("create_model_from_file does NOT call clean_modelfit_data when data is NULL", {
  model_code <- c("$PROBLEM Test", "$PRED Y = THETA(1)", "$THETA 1")
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  mock_model <- structure(list(), class = "pharmpy.model.model.Model")
  clean_mock <- mockery::mock()

  mockery::stub(create_model_from_file, "pharmr::read_model_from_string", mock_model)
  mockery::stub(create_model_from_file, "clean_modelfit_data", clean_mock)

  create_model_from_file(model_file = tmp_mod, data = NULL)

  expect_length(mockery::mock_calls(clean_mock), 0L)

  unlink(tmp_mod)
})

test_that("create_model_from_file converts numeric-as-character column to numeric in dataset", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")

  test_data <- data.frame(
    ID = 1L,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1L,
    EVID = c(1L, 0L, 0L),
    MDV = c(1L, 0L, 0L),
    WT = c("70", "70", "70")  # numeric value stored as character
  )

  result <- create_model_from_file(
    model_file = mod_file,
    data = test_data,
    verbose = FALSE
  )

  expect_s3_class(result, "pharmpy.model.model.Model")
  expect_true(is.numeric(result$dataset$WT))
  expect_equal(result$dataset$WT, c(70, 70, 70))
})

test_that("create_model_from_file handles multiple character columns in dataset", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")

  test_data <- data.frame(
    ID = 1L,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1L,
    EVID = c(1L, 0L, 0L),
    MDV = c(1L, 0L, 0L),
    WT = c("70", "70", "70"),   # character
    AGE = c("30", "30", "30")   # character
  )

  result <- create_model_from_file(
    model_file = mod_file,
    data = test_data,
    verbose = FALSE
  )

  expect_s3_class(result, "pharmpy.model.model.Model")
  expect_true(is.numeric(result$dataset$WT))
  expect_true(is.numeric(result$dataset$AGE))
  expect_equal(result$dataset$WT, c(70, 70, 70))
  expect_equal(result$dataset$AGE, c(30, 30, 30))
})

test_that("create_model_from_file circumvents bug in pharmpy with dummy_eta and can add peripheral comparment", {
  local_pharmr.extra_options()
  model <- create_model_from_file(test_path("fixtures", "model_with_dummyeta", "run1.mod"))
  mod2 <- model |>
    pharmr::add_peripheral_compartment()
  expect_s3_class(mod2, "pharmpy.model.model.Model")
  expect_true(all(c("POP_QP1", "POP_VP1") %in% mod2$parameters$names))
})

test_that("strip_input_commas replaces commas in $INPUT with spaces", {
  expect_equal(
    strip_input_commas("$INPUT ID, TIME, DV, AMT"),
    "$INPUT ID  TIME  DV  AMT"
  )
})

test_that("strip_input_commas leaves comma-free $INPUT untouched", {
  expect_equal(
    strip_input_commas("$INPUT ID TIME DV AMT"),
    "$INPUT ID TIME DV AMT"
  )
})

test_that("strip_input_commas only touches $INPUT, not other records", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID, TIME, DV, AMT",
    "$DATA data.csv",
    "$THETA (0, 1), (0, 2)",
    "$OMEGA 0.1, 0.2",
    sep = "\n"
  )
  out <- strip_input_commas(code)
  expect_true(grepl("$INPUT ID  TIME  DV  AMT", out, fixed = TRUE))
  expect_true(grepl("$THETA (0, 1), (0, 2)", out, fixed = TRUE))
  expect_true(grepl("$OMEGA 0.1, 0.2", out, fixed = TRUE))
})

test_that("strip_input_commas handles multi-line $INPUT", {
  code <- "$INPUT ID, TIME,\n  DV, AMT\n$DATA data.csv, IGNORE=@"
  out <- strip_input_commas(code)
  expect_true(grepl("$INPUT ID  TIME \n  DV  AMT", out, fixed = TRUE))
  expect_true(grepl("$DATA data.csv, IGNORE=@", out, fixed = TRUE))
})

test_that("strip_input_commas leaves commas in a leading comment untouched", {
  code <- "; PK model, final\n$PROBLEM Test\n$INPUT ID, TIME, DV"
  out <- strip_input_commas(code)
  expect_true(grepl("; PK model, final", out, fixed = TRUE))
  expect_true(grepl("$INPUT ID  TIME  DV", out, fixed = TRUE))
})

test_that("preserves non-numeric DROP columns when data is a data.frame (#101)", {
  local_pharmr.extra_options()

  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    AMT = c(100, 0, 0, 100, 0, 0),
    DV = c(0, 10, 5, 0, 12, 6),
    MDV = c(1, 0, 0, 1, 0, 0),
    # Non-numeric date/time-of-day columns NONMEM ignores via $INPUT DROP.
    # These crash if the dataset round-trip rewrites $INPUT and loses the
    # DROP flags, because pharmpy then tries to float-convert them.
    VISITDATE = c("08/12/2011", "08/13/2011", "08/14/2011",
                  "09/01/2011", "09/02/2011", "09/03/2011"),
    CLOCK = c("9:00", "10:00", "11:00", "9:00", "10:00", "11:00")
  )
  model_code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME AMT DV MDV VISITDATE=DROP CLOCK=DROP",
    "$DATA DUMMYPATH IGNORE=@",
    "$SUBROUTINE ADVAN1 TRANS2",
    "$PK",
    "CL=THETA(1)*EXP(ETA(1))",
    "V=THETA(2)",
    "S1=V",
    "$ERROR",
    "Y=F+F*EPS(1)",
    "$THETA (0,1)",
    "$THETA (0,10)",
    "$OMEGA 0.1",
    "$SIGMA 0.1",
    "$ESTIMATION METHOD=1",
    sep = "\n"
  )
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)

  model <- expect_no_error(create_model_from_file(tmp_mod, data = dat))

  # $INPUT (and its DROP flags) must survive untouched:
  expect_true(grepl("VISITDATE=DROP", model$code, fixed = TRUE))
  expect_true(grepl("CLOCK=DROP", model$code, fixed = TRUE))
  # $DATA points at the temp CSV, not DUMMYPATH:
  expect_false(grepl("DUMMYPATH", model$code, fixed = TRUE))
  # Dataset is materializable and keeps the non-numeric columns intact:
  expect_false(is.null(model$dataset))
  expect_true(all(c("VISITDATE", "CLOCK") %in% names(model$dataset)))
})

test_that("sync_input_to_dataset preserves DROP flags and appends new columns", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME AMT DV MDV VISITDATE=DROP",
    "$DATA data.csv IGNORE=@",
    sep = "\n"
  )
  out <- sync_input_to_dataset(
    code, c("ID", "TIME", "AMT", "DV", "MDV", "VISITDATE", "WT")
  )
  expect_equal(
    get_input_tokens(out),
    c("ID", "TIME", "AMT", "DV", "MDV", "VISITDATE=DROP", "WT")
  )
  # Other records are untouched:
  expect_true(grepl("$DATA data.csv IGNORE=@", out, fixed = TRUE))
})

test_that("sync_input_to_dataset follows dataset column order", {
  code <- "$PROBLEM Test\n$INPUT ID TIME DV CLOCK=DROP AMT\n$DATA data.csv"
  out <- sync_input_to_dataset(code, c("ID", "CLOCK", "TIME", "AMT", "DV"))
  expect_equal(
    get_input_tokens(out), c("ID", "CLOCK=DROP", "TIME", "AMT", "DV")
  )
})

test_that("sync_input_to_dataset handles anonymous DROP and DROP=<col>", {
  code <- "$PROBLEM Test\n$INPUT ID TIME DV DROP DROP=CLOCK\n$DATA data.csv"
  out <- sync_input_to_dataset(code, c("ID", "TIME", "DV", "_DROP1", "CLOCK"))
  expect_equal(
    get_input_tokens(out), c("ID", "TIME", "DV", "DROP", "DROP=CLOCK")
  )
})

test_that("sync_input_to_dataset is a no-op without $INPUT or columns", {
  code <- "$PROBLEM Test\n$DATA data.csv"
  expect_equal(sync_input_to_dataset(code, c("ID", "TIME")), code)
  code_with_input <- "$PROBLEM Test\n$INPUT ID TIME DV\n$DATA data.csv"
  expect_equal(sync_input_to_dataset(code_with_input, character(0)),
               code_with_input)
})

test_that("sync_input_to_dataset keeps labelled columns and anonymous DROPs in place", {
  # $INPUT as set_dv() leaves it: old DV demoted to an anonymous DROP, new DV
  # declared as a synonym. The dataset still carries the original `DV` name.
  code <- "$PROBLEM Test\n$INPUT ID TIME DROP AMT EVID MDV DV=CONC\n$DATA data.csv"
  out <- sync_input_to_dataset(
    code, c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "CONC")
  )
  expect_equal(
    get_input_tokens(out),
    c("ID", "TIME", "DROP", "AMT", "EVID", "MDV", "DV=CONC")
  )
})

test_that("input_token_name resolves plain, labelled and DROP tokens", {
  expect_equal(input_token_name("WT"), "WT")
  expect_equal(input_token_name("DV=CONC"), "CONC")
  expect_equal(input_token_name("VISITDATE=DROP"), "VISITDATE")
  expect_equal(input_token_name("DROP=VISITDATE"), "VISITDATE")
  expect_equal(input_token_name("CLOCK=SKIP"), "CLOCK")
})
