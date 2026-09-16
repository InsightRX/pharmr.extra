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

test_that("create_model_from_file drops a bookkeeping column with an invalid NONMEM name", {
  local_pharmr.extra_options()
  mod_file <- testthat::test_path("fixtures", "run_with_ext", "run.mod")

  # `.regimen` is what create_sim_dataset() attaches; its leading dot is an
  # invalid $INPUT symbol, so a bare (or even DROP-flagged) token makes
  # Pharmpy's parser reject the model. It must be dropped from the dataset.
  test_data <- data.frame(
    ID = 1L,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1L,
    EVID = c(1L, 0L, 0L),
    MDV = c(1L, 0L, 0L),
    check.names = FALSE
  )
  test_data[[".regimen"]] <- c(1, 1, 1)

  result <- create_model_from_file(
    model_file = mod_file,
    data = test_data,
    verbose = FALSE
  )

  expect_s3_class(result, "pharmpy.model.model.Model")
  expect_false(".regimen" %in% names(result$dataset))
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

test_that("sync_input_to_dataset drops a genuinely new non-numeric column", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME AMT DV MDV",
    "$DATA data.csv IGNORE=@",
    sep = "\n"
  )
  # WT is numeric (append bare), TRT is a new text column (must be dropped so
  # Pharmpy does not try to float-convert its labels).
  out <- sync_input_to_dataset(
    code,
    c("ID", "TIME", "AMT", "DV", "MDV", "WT", "TRT"),
    non_numeric = "TRT"
  )
  expect_equal(
    get_input_tokens(out),
    c("ID", "TIME", "AMT", "DV", "MDV", "WT", "TRT=DROP")
  )
})

test_that("sync_input_to_dataset re-emits pharmpy's _DROP placeholders", {
  code <- "$PROBLEM Test\n$INPUT ID TIME DROP AMT DV\n$DATA data.csv"
  out <- sync_input_to_dataset(
    code,
    c("ID", "TIME", "_DROP1", "AMT", "DV"),
    non_numeric = character(0)
  )
  expect_equal(get_input_tokens(out), c("ID", "TIME", "DROP", "AMT", "DV"))
})

test_that("blank_unreadable_values blanks only values NONMEM cannot read", {
  data <- data.frame(
    ID = c(1, 2, 3),
    TRT = c("Cohort A", "EV+P", "B,C"),
    VISITDATE = c("08/12/2011", "09/12/2011", "10/12/2011"),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(
    blank_unreadable_values(data, non_numeric = c("TRT", "VISITDATE"))
  )
  # whitespace and comma values are unreadable; "EV+P" and the dates are fine
  expect_equal(out$TRT, c(".", "EV+P", "."))
  expect_equal(out$VISITDATE, data$VISITDATE)
  # numeric columns are untouched
  expect_equal(out$ID, data$ID)
})

test_that("blank_unreadable_values leaves a clean dataset identical", {
  data <- data.frame(ID = c(1, 2), TRT = c("A", "B"), stringsAsFactors = FALSE)
  expect_identical(blank_unreadable_values(data, non_numeric = "TRT"), data)
})

test_that("is_numeric_column treats numeric-as-character as numeric", {
  expect_true(is_numeric_column(c(1, 2, 3)))
  expect_true(is_numeric_column(c("70", "70", "70")))
  expect_true(is_numeric_column(c("1", ".", "", NA)))   # NONMEM missing markers
  expect_false(is_numeric_column(c("Cohort A", "EV+P")))
  expect_false(is_numeric_column(c("70", "EV+P")))
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

for (input_kind in c("frame", "quoted CSV", "unquoted CSV")) {
  for (data_options in c("", "IGNORE=@", "IGNORE='#'", "IGNORE=(ID.EQ.2)",
                         "ACCEPT=(ID.EQ.1)", "IGNORE=@\n IGNORE=(ID.EQ.2)")) {
    test_that(paste("dataset binding handles", input_kind, "with", data_options), {
      local_pharmr.extra_options()
      tmp <- withr::local_tempdir()
      dat <- data.frame(ID = c(1, 1, 2), TIME = c(0, 1, 0),
                        CONC = c(0, 2.5, 4), CLOCK = c("9:00", "10:00", "9:00"))
      code <- paste("$PROBLEM Header binding", "$INPUT ID TIME DV=CONC CLOCK=DROP",
                    paste("$DATA dummy", data_options, "; retain data options"),
                    "$PRED Y=THETA(1)+EPS(1)", "$THETA 1", "$SIGMA 1",
                    "$ESTIMATION METHOD=0 MAXEVAL=0", sep = "\n")
      mod_file <- file.path(tmp, "model.mod")
      writeLines(code, mod_file)
      csv_file <- file.path(tmp, "input.csv")
      write.csv(dat, csv_file, row.names = FALSE, quote = input_kind != "unquoted CSV")
      original <- readBin(csv_file, "raw", n = file.info(csv_file)$size)
      supplied <- if (input_kind == "frame") dat else csv_file

      model <- create_model_from_file(mod_file, data = supplied, verbose = FALSE)

      retained <- if (grepl("ID.EQ", data_options, fixed = TRUE)) c(1, 2) else 1:3
      expect_equal(model$dataset$ID, dat$ID[retained])
      expect_equal(model$dataset$CONC, dat$CONC[retained])
      expect_equal(names(model$dataset), names(dat))
      expect_true(grepl("DV=CONC CLOCK=DROP", model$code, fixed = TRUE))
      if (grepl("ID.EQ", data_options, fixed = TRUE)) {
        filter <- if (grepl("ACCEPT", data_options)) "ACCEPT=(ID.EQ.1)" else "IGNORE=(ID.EQ.2)"
        expect_true(grepl(filter, model$code, fixed = TRUE))
      }
      expect_equal(readBin(csv_file, "raw", n = file.info(csv_file)$size), original)
      expect_equal(readLines(mod_file), strsplit(code, "\n", fixed = TRUE)[[1]])
      copy <- as.character(model$datainfo$path)
      expect_false(identical(copy, csv_file))
      expect_equal(read.csv(copy, col.names = names(dat), check.names = FALSE), dat)
      writeLines(model$code, file.path(tmp, "bound.mod"))
      rebound <- create_model_from_file(file.path(tmp, "bound.mod"), data = copy, verbose = FALSE)
      expect_equal(lapply(rebound$dataset, identity), lapply(model$dataset, identity))
      prepared <- prepare_run_folder("prepared", model, tmp, data = dat, verbose = FALSE)
      ready <- pharmr::read_model(file.path(prepared$fit_folder, prepared$model_file))
      expect_equal(lapply(ready$dataset, identity), lapply(model$dataset, identity))
    })
  }
}

test_that("dataset binding preserves CSV null tokens and numeric text", {
  local_pharmr.extra_options()
  tmp <- withr::local_tempdir()
  mod_file <- file.path(tmp, "model.mod")
  csv_file <- file.path(tmp, "input.csv")
  writeLines(c("$PROBLEM Null tokens", "$INPUT ID TIME DV",
               "$DATA dummy NULL=9 IGNORE=@", "$PRED Y=THETA(1)+EPS(1)",
               "$THETA 1", "$SIGMA 1"), mod_file)
  writeLines(c('"ID","TIME","DV"', '001,0,.', '001,1,',
               '001,2,1.2345678901234567'), csv_file)

  model <- create_model_from_file(mod_file, data = csv_file, verbose = FALSE)

  expect_equal(model$dataset$DV, c(9, 9, 1.2345678901234567))
  expect_true(grepl("NULL=9", model$code, fixed = TRUE))
  expect_equal(readLines(as.character(model$datainfo$path))[-1], readLines(csv_file)[-1])
})

test_that("dataset binding writes missing frame values as NONMEM nulls", {
  local_pharmr.extra_options()
  tmp <- withr::local_tempdir()
  mod_file <- file.path(tmp, "model.mod")
  writeLines(c("$PROBLEM Missing values", "$INPUT ID TIME DV", "$DATA dummy NULL=9",
               "$PRED Y=THETA(1)+EPS(1)", "$THETA 1", "$SIGMA 1"), mod_file)

  model <- create_model_from_file(
    mod_file, data = data.frame(ID = 1, TIME = c(0, 1), DV = c(NA, 2)), verbose = FALSE
  )

  expect_equal(model$dataset$DV, c(9, 2))
})

test_that("run_sim binds a model filename without a header skip before execution", {
  local_pharmr.extra_options()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  code <- sub("IGNORE=@", "", make_model_without_cov()$code, fixed = TRUE)
  writeLines(code, "model.mod")
  received <- NULL
  local_mocked_bindings(
    run_nlme = function(model, ...) {
      received <<- model$dataset
      .mock_nlme_result()
    },
    .package = "pharmr.extra"
  )

  out <- run_sim(model = "model.mod", data = .sim_dat(), update_table = FALSE,
                 verbose = FALSE)

  expect_equal(received$ID, .sim_dat()$ID)
  expect_equal(received$DV, .sim_dat()$DV)
  expect_equal(nrow(out), 3)
})

for (first_column in c("INDEX", "_DROP1")) {
  for (marker in c("I", "_", "2", "#", "")) {
    test_that(paste("dataset binding preserves leading DROP data with", first_column, marker), {
      local_pharmr.extra_options()
      tmp <- withr::local_tempdir()
      dat <- data.frame(INDEX = c("A", "9", "2"), ID = 1:3, TIME = 0, DV = 1:3)
      names(dat)[1] <- first_column
      input <- if (first_column == "INDEX") "INDEX=DROP" else "DROP"
      options <- if (nzchar(marker)) paste0("IGNORE=", marker) else ""
      code <- paste("$PROBLEM Leading dropped column", paste("$INPUT", input, "ID TIME DV"),
                    paste("$DATA dummy", options), "$PRED Y=THETA(1)+EPS(1)",
                    "$THETA 1", "$SIGMA 1", sep = "\n")
      mod_file <- file.path(tmp, "model.mod")
      writeLines(code, mod_file)

      model <- create_model_from_file(mod_file, data = dat, verbose = FALSE)

      rows <- if(marker == "2") 1:2 else 1:3
      expect_equal(model$dataset$ID, dat$ID[rows])
      expect_equal(model$dataset[[first_column]], dat[[first_column]][rows])
      writeLines(model$code, mod_file)
      rebound <- create_model_from_file(mod_file, data = as.character(model$datainfo$path), verbose = FALSE)
      expect_equal(lapply(rebound$dataset, identity), lapply(model$dataset, identity))
      prepared <- prepare_run_folder("prepared", model, tmp, data = dat, verbose = FALSE)
      ready <- pharmr::read_model(file.path(prepared$fit_folder, prepared$model_file))
      expect_equal(lapply(ready$dataset, identity), lapply(model$dataset, identity))
    })
  }
}

for (marker in c('"', ',')) {
  test_that(paste("dataset binding round-trips CSV punctuation IGNORE", marker), {
    local_pharmr.extra_options()
    tmp <- withr::local_tempdir()
    mod_file <- file.path(tmp, "model.mod")
    writeLines(c("$PROBLEM Quoted header", "$INPUT ID TIME DV",
                 paste0("$DATA dummy IGNORE='", marker, "'"),
                 "$PRED Y=THETA(1)+EPS(1)", "$THETA 1", "$SIGMA 1"), mod_file)
    dat <- data.frame(ID = c(1, 2), TIME = 0, DV = c(2, 3))

    model <- create_model_from_file(mod_file, data = dat, verbose = FALSE)
    writeLines(model$code, mod_file)
    rebound <- create_model_from_file(mod_file, data = as.character(model$datainfo$path), verbose = FALSE)

    expect_equal(lapply(rebound$dataset, identity), lapply(dat, identity))
  })
}
