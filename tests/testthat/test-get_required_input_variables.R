# Minimal reusable model snippets -------------------------------------------

# Simple model: reserved cols + one used covariate (WT) + one unused (AGE)
simple_code <- paste(
  "$PROBLEM Test",
  "$INPUT ID TIME DV AMT EVID MDV WT AGE",
  "$DATA data.csv IGNORE=@",
  "$SUBROUTINES ADVAN1 TRANS2",
  "$PK",
  "CL = THETA(1) * (WT/70)",
  "V  = THETA(2)",
  "S1 = V",
  "$ERROR",
  "Y = F + EPS(1)",
  "$THETA (0,10) (0,50)",
  "$OMEGA 0.1",
  "$SIGMA 0.1",
  "$ESTIMATION METHOD=1",
  sep = "\n"
)

# Model with $INPUT renames and DROP columns
rename_code <- paste(
  "$PROBLEM Rename test",
  "$INPUT ID TIME DV=LNDV AMT EVID MDV WT=WEIGHT RACE=DROP SMOKE",
  "$DATA data.csv IGNORE=@",
  "$PK",
  "CL = THETA(1) * (WT/70)",
  "V  = THETA(2)",
  "S1 = V",
  "$ERROR",
  "Y = F + EPS(1)*F",
  "$THETA (0,10) (0,50)",
  "$OMEGA 0.1",
  "$SIGMA 0.1",
  "$ESTIMATION METHOD=1",
  sep = "\n"
)

# Model with dose-timing variables
dose_var_code <- paste(
  "$PROBLEM Dose var test",
  "$INPUT ID TIME DV AMT EVID MDV DUR ALAG",
  "$DATA data.csv IGNORE=@",
  "$PK",
  "CL = THETA(1)",
  "V  = THETA(2)",
  "S1 = V",
  "D1   = DUR",
  "ALAG1 = ALAG * 24",
  "$ERROR",
  "Y = F + EPS(1)",
  "$THETA (0,10) (0,50) (0,1) (0,1)",
  "$OMEGA 0.1 0.1",
  "$SIGMA 0.1",
  "$ESTIMATION METHOD=1",
  sep = "\n"
)

# Model with IGNORE=C
ignore_c_code <- paste(
  "$PROBLEM IGNORE C test",
  "$INPUT C ID TIME DV AMT EVID MDV",
  "$DATA data.csv IGNORE=C",
  "$PK",
  "CL = THETA(1)",
  "V  = THETA(2)",
  "S1 = V",
  "$ERROR",
  "Y = F + EPS(1)",
  "$THETA (0,10) (0,50)",
  "$OMEGA 0.1",
  "$SIGMA 0.1",
  "$ESTIMATION METHOD=1",
  sep = "\n"
)

# Basic output structure -----------------------------------------------------

test_that("returns a data.frame with expected columns", {
  out <- get_required_input_variables(simple_code)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("nonmem_name", "data_col", "type", "required"))
})

test_that("has one row per $INPUT token", {
  out <- get_required_input_variables(simple_code)
  expect_equal(nrow(out), 8L)  # ID TIME DV AMT EVID MDV WT AGE
})

# Column classification -------------------------------------------------------

test_that("classifies reserved NONMEM columns correctly", {
  out <- get_required_input_variables(simple_code)
  reserved_names <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV")
  expect_equal(out$type[out$nonmem_name %in% reserved_names], rep("reserved", 6))
  expect_true(all(out$required[out$nonmem_name %in% reserved_names]))
})

test_that("classifies used covariate correctly", {
  out <- get_required_input_variables(simple_code)
  expect_equal(out$type[out$nonmem_name == "WT"], "used_covariate")
  expect_true(out$required[out$nonmem_name == "WT"])
})

test_that("classifies unused covariate correctly", {
  out <- get_required_input_variables(simple_code)
  expect_equal(out$type[out$nonmem_name == "AGE"], "unused_covariate")
  expect_false(out$required[out$nonmem_name == "AGE"])
})

test_that("classifies dose-timing variables correctly", {
  out <- get_required_input_variables(dose_var_code)
  expect_equal(out$type[out$nonmem_name == "DUR"],  "dose_variable")
  expect_equal(out$type[out$nonmem_name == "ALAG"], "dose_variable")
  expect_true(out$required[out$nonmem_name == "DUR"])
  expect_true(out$required[out$nonmem_name == "ALAG"])
})

test_that("classifies NAME=DROP columns as dropped and not required", {
  out <- get_required_input_variables(rename_code)
  expect_equal(out$type[out$nonmem_name == "RACE"], "dropped")
  expect_false(out$required[out$nonmem_name == "RACE"])
})

test_that("positional anonymous DROP is classified as dropped", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME DV AMT EVID DROP",
    "$PK\nCL = THETA(1)\nV = THETA(2)\nS1 = V",
    "$ERROR\nY = F + EPS(1)",
    "$THETA (0,10) (0,50)\n$OMEGA 0.1\n$SIGMA 0.1",
    "$ESTIMATION METHOD=1",
    sep = "\n"
  )
  out <- get_required_input_variables(code)
  expect_equal(out$type[6], "dropped")
  expect_false(out$required[6])
})

# IGNORE=C adds C to reserved -------------------------------------------------

test_that("IGNORE=C causes C column to be classified as reserved", {
  out <- get_required_input_variables(ignore_c_code)
  expect_equal(out$type[out$nonmem_name == "C"], "reserved")
  expect_true(out$required[out$nonmem_name == "C"])
})

test_that("C column is not reserved when IGNORE=@ is used", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT C ID TIME DV AMT EVID MDV",
    "$DATA data.csv IGNORE=@",
    "$PK\nCL = THETA(1)\nV = THETA(2)\nS1 = V",
    "$ERROR\nY = F + EPS(1)",
    "$THETA (0,10) (0,50)\n$OMEGA 0.1\n$SIGMA 0.1",
    "$ESTIMATION METHOD=1",
    sep = "\n"
  )
  out <- get_required_input_variables(code)
  # C is not referenced in $PK/$ERROR and IGNORE=@ → unused_covariate
  expect_equal(out$type[out$nonmem_name == "C"], "unused_covariate")
})

# include_reserved_nonmem -----------------------------------------------------

test_that("include_reserved_nonmem = FALSE excludes reserved columns", {
  out <- get_required_input_variables(simple_code, include_reserved_nonmem = FALSE)
  reserved <- c("ID", "TIME", "DV", "AMT", "EVID", "MDV")
  expect_false(any(out$nonmem_name %in% reserved))
  expect_true("WT"  %in% out$nonmem_name)
  expect_true("AGE" %in% out$nonmem_name)
})

test_that("include_reserved_nonmem = TRUE includes reserved columns (default)", {
  out <- get_required_input_variables(simple_code, include_reserved_nonmem = TRUE)
  expect_true("ID" %in% out$nonmem_name)
  expect_true("DV" %in% out$nonmem_name)
})

# data_col defaults -----------------------------------------------------------

test_that("data_col equals nonmem_name when no data argument given and $DATA file absent", {
  out <- get_required_input_variables(simple_code)
  expect_equal(out$data_col, out$nonmem_name)
})

test_that("nonmem_name is the NONMEM internal name, not the data column, for renames", {
  out <- get_required_input_variables(rename_code)
  # DV=LNDV → NONMEM calls it DV; without data argument data_col also shows DV
  expect_equal(out$nonmem_name[out$nonmem_name == "DV"], "DV")
  expect_equal(out$nonmem_name[out$nonmem_name == "WT"], "WT")
})

# data argument: data.frame ---------------------------------------------------

test_that("data_col is set from data.frame column names positionally", {
  df <- data.frame(
    PATID = 1, T = 1, CONC = 1, DOSE = 1, EVIDX = 1, MISS = 1,
    WEIGHT = 1, AGEBL = 1
  )
  out <- get_required_input_variables(simple_code, data = df)
  expect_equal(out$data_col, c("PATID", "T", "CONC", "DOSE", "EVIDX", "MISS", "WEIGHT", "AGEBL"))
  # nonmem_name unchanged
  expect_equal(out$nonmem_name, c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "WT", "AGE"))
})

test_that("data.frame with more columns than $INPUT trims to $INPUT length", {
  df <- data.frame(
    PATID = 1, T = 1, CONC = 1, DOSE = 1, EVIDX = 1, MISS = 1,
    WEIGHT = 1, AGEBL = 1, EXTRA = 1  # 9 cols, $INPUT has 8
  )
  out <- get_required_input_variables(simple_code, data = df)
  expect_equal(nrow(out), 8L)
  expect_equal(out$data_col[1], "PATID")
  expect_equal(out$data_col[8], "AGEBL")  # EXTRA not included
})

test_that("data.frame with fewer columns than $INPUT warns and falls back to nonmem_name", {
  df <- data.frame(PATID = 1, T = 1, CONC = 1)  # only 3 cols, $INPUT has 8
  expect_warning(
    out <- get_required_input_variables(simple_code, data = df),
    "cannot match positionally"
  )
  expect_equal(out$data_col, out$nonmem_name)
})

# data argument: CSV file -----------------------------------------------------

test_that("data_col is set from CSV file column names positionally", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(
    data.frame(PATID=1, T=1, CONC=1, DOSE=1, EVIDX=1, MISS=1, WEIGHT=1, AGEBL=1),
    tmp, row.names = FALSE
  )
  out <- get_required_input_variables(simple_code, data = tmp)
  expect_equal(out$data_col, c("PATID", "T", "CONC", "DOSE", "EVIDX", "MISS", "WEIGHT", "AGEBL"))
})

test_that("errors when explicit data file path does not exist", {
  expect_error(
    get_required_input_variables(simple_code, data = "/nonexistent/path/data.csv"),
    "File not found"
  )
})

test_that("errors when data argument is not a data.frame or file path", {
  expect_error(
    get_required_input_variables(simple_code, data = 42),
    "`data` must be a data.frame or a path to a CSV file"
  )
})

# $DATA auto-detection --------------------------------------------------------

test_that("auto-detects $DATA file relative to model file", {
  tmp_dir  <- tempdir()
  mod_file <- file.path(tmp_dir, "run.mod")
  dat_file <- file.path(tmp_dir, "data.csv")
  on.exit({ unlink(mod_file); unlink(dat_file) })

  writeLines(simple_code, mod_file)
  write.csv(
    data.frame(PATID=1, T=1, CONC=1, DOSE=1, EVIDX=1, MISS=1, WEIGHT=1, AGEBL=1),
    dat_file, row.names = FALSE
  )

  out <- get_required_input_variables(mod_file)
  expect_equal(out$data_col, c("PATID", "T", "CONC", "DOSE", "EVIDX", "MISS", "WEIGHT", "AGEBL"))
})

test_that("warns and falls back when $DATA file is not found", {
  tmp <- tempfile(fileext = ".mod")
  on.exit(unlink(tmp))
  writeLines(simple_code, tmp)  # $DATA points to "data.csv" which won't exist in tmpdir

  expect_warning(
    out <- get_required_input_variables(tmp),
    "not found"
  )
  expect_equal(out$data_col, out$nonmem_name)
})

test_that("$DATA auto-detection handles more data cols than $INPUT entries", {
  tmp_dir  <- tempdir()
  mod_file <- file.path(tmp_dir, "run2.mod")
  dat_file <- file.path(tmp_dir, "data2.csv")
  on.exit({ unlink(mod_file); unlink(dat_file) })

  code2 <- sub("\\$DATA data.csv", "$DATA data2.csv", simple_code)
  writeLines(code2, mod_file)
  # 9 columns, $INPUT expects 8
  write.csv(
    data.frame(PATID=1, T=1, CONC=1, DOSE=1, EVIDX=1, MISS=1, WEIGHT=1, AGEBL=1, EXTRA=1),
    dat_file, row.names = FALSE
  )

  out <- get_required_input_variables(mod_file)
  expect_equal(nrow(out), 8L)
  expect_equal(out$data_col[8], "AGEBL")
})

# Model input: code string vs file path ---------------------------------------

test_that("accepts a model file path", {
  tmp <- tempfile(fileext = ".mod")
  on.exit(unlink(tmp))
  writeLines(simple_code, tmp)
  out <- suppressWarnings(get_required_input_variables(tmp))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 8L)
})

test_that("errors when model is not a string, file, or Pharmpy object", {
  expect_error(
    get_required_input_variables(123),
    "`model` must be"
  )
  expect_error(
    get_required_input_variables(list()),
    "`model` must be"
  )
})

# Pharmpy model object --------------------------------------------------------

test_that("accepts a Pharmpy model object", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()
  pharmpy_model <- pharmr::read_model_from_string(simple_code)
  out <- get_required_input_variables(pharmpy_model)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 8L)
  expect_true("WT" %in% out$nonmem_name)
})

# Inline comments in $INPUT ---------------------------------------------------

test_that("strips inline comments in $INPUT", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME DV ; dependent variable",
    "  AMT EVID MDV",
    "$PK\nCL = THETA(1)\nV = THETA(2)\nS1 = V",
    "$ERROR\nY = F + EPS(1)",
    "$THETA (0,10) (0,50)\n$OMEGA 0.1\n$SIGMA 0.1",
    "$ESTIMATION METHOD=1",
    sep = "\n"
  )
  out <- get_required_input_variables(code)
  expect_equal(out$nonmem_name, c("ID", "TIME", "DV", "AMT", "EVID", "MDV"))
})

# Multi-line $INPUT -----------------------------------------------------------

test_that("handles $INPUT split across multiple lines", {
  code <- paste(
    "$PROBLEM Test",
    "$INPUT ID TIME",
    "  DV AMT",
    "  EVID MDV WT AGE",
    "$DATA data.csv IGNORE=@",
    "$PK\nCL = THETA(1) * (WT/70)\nV = THETA(2)\nS1 = V",
    "$ERROR\nY = F + EPS(1)",
    "$THETA (0,10) (0,50)\n$OMEGA 0.1\n$SIGMA 0.1",
    "$ESTIMATION METHOD=1",
    sep = "\n"
  )
  out <- get_required_input_variables(code)
  expect_equal(out$nonmem_name, c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "WT", "AGE"))
})
