base_code <- paste0(
  "$PROBLEM Test\n",
  "$INPUT ID TIME DV AMT EVID MDV BW\n",
  "$DATA data.csv IGNORE=@\n",
  "$SUBROUTINES ADVAN1 TRANS2\n",
  "$PK\nBW_EFF = BW / 70\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
  "$ERROR\nY=F+EPS(1)\n",
  "$THETA (0,10) ; POP_CL\n",
  "$THETA (0,50) ; POP_V\n",
  "$SIGMA 0.1\n",
  "$EST METHOD=1\n"
)

get_input_line <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  paste(grep("^\\$IN", lines, value = TRUE), collapse = " ")
}

# ---- drop_input_columns ----

test_that("drop_input_columns replaces column with DROP in $INPUT", {
  model  <- pharmr::read_model_from_string(base_code)
  result <- drop_input_columns(model, "BW")

  input_line <- get_input_line(result$code)
  expect_match(input_line, "DROP", fixed = TRUE)
  # BW must not appear as a standalone token any more
  expect_false(grepl("(?<![=A-Za-z0-9_])BW(?!=)", input_line, perl = TRUE))
})

test_that("drop_input_columns drops multiple columns at once", {
  model  <- pharmr::read_model_from_string(base_code)
  result <- drop_input_columns(model, c("BW", "MDV"))

  input_line <- get_input_line(result$code)
  expect_match(input_line, "DROP",  fixed = TRUE)
  expect_match(input_line, "DROP", fixed = TRUE)
  expect_false(grepl("(?<![=A-Za-z0-9_])BW(?!=)",  input_line, perl = TRUE))
  expect_false(grepl("(?<![=A-Za-z0-9_])MDV(?!=)", input_line, perl = TRUE))
})

test_that("drop_input_columns does not modify occurrences outside $INPUT", {
  model  <- pharmr::read_model_from_string(base_code)
  result <- drop_input_columns(model, "BW")

  code_lines <- strsplit(result$code, "\n", fixed = TRUE)[[1]]
  in_pk <- FALSE
  pk_bw_present <- FALSE
  for (ln in code_lines) {
    stripped <- trimws(ln)
    if (startsWith(stripped, "$")) {
      in_pk <- grepl("^\\$PK($|\\s)", stripped)
    }
    if (in_pk && grepl("\\bBW\\b", ln)) {
      pk_bw_present <- TRUE
    }
  }
  expect_true(pk_bw_present, label = "BW should still appear in $PK after being dropped from $INPUT")
})

test_that("drop_input_columns does not replace the right-hand side of aliases", {
  # $INPUT contains DV=CONC — CONC is an alias target, not a standalone column
  aliased_code <- paste0(
    "$PROBLEM Test\n",
    "$INPUT ID TIME DV=CONC AMT EVID MDV BW\n",
    "$DATA data.csv IGNORE=@\n",
    "$SUBROUTINES ADVAN1 TRANS2\n",
    "$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
    "$ERROR\nY=F+EPS(1)\n",
    "$THETA (0,10) ; POP_CL\n",
    "$THETA (0,50) ; POP_V\n",
    "$SIGMA 0.1\n",
    "$EST METHOD=1\n"
  )
  model  <- pharmr::read_model_from_string(aliased_code)
  result <- drop_input_columns(model, "BW")

  expect_match(result$code, "DV=CONC", fixed = TRUE)
})

test_that("drop_input_columns returns a valid Pharmpy model object", {
  model  <- pharmr::read_model_from_string(base_code)
  result <- drop_input_columns(model, "BW")

  expect_true(inherits(result, "pharmpy.model.model.Model"))
})

# ---- $INPUT abbreviation detection ----

test_that("drop_input_columns handles abbreviated $INPUT record names", {
  for (abbrev in c("$IN", "$INP", "$INPU", "$INPUT")) {
    abbrev_code <- sub("$INPUT", abbrev, base_code, fixed = TRUE)
    model  <- pharmr::read_model_from_string(abbrev_code)
    result <- drop_input_columns(model, "BW")
    input_line <- get_input_line(result$code)
    expect_match(
      input_line, "DROP", fixed = TRUE,
      label = paste0("DROP present for abbreviated record '", abbrev, "'")
    )
  }
})

# ---- create_model() validation ----

test_that("create_model errors when drop_input is not a character vector", {
  test_data <- data.frame(
    ID = 1L, TIME = c(0, 1, 2), DV = c(0, 10, 5), CMT = 1L,
    AMT = c(100, 0, 0), EVID = c(1L, 0L, 0L), MDV = c(1L, 0L, 0L), BW = 70
  )
  expect_error(
    create_model(route = "iv", data = test_data, drop_input = 123),
    "`drop_input` needs to be a character vector"
  )
})

test_that("create_model errors when drop_input is used without a data.frame", {
  expect_error(
    create_model(route = "iv", drop_input = "BW"),
    "`drop_input` can only be used when `data` is supplied"
  )
})

test_that("create_model errors when drop_input names a column absent from the dataset", {
  test_data <- data.frame(
    ID = 1L, TIME = c(0, 1, 2), DV = c(0, 10, 5), CMT = 1L,
    AMT = c(100, 0, 0), EVID = c(1L, 0L, 0L), MDV = c(1L, 0L, 0L)
  )
  expect_error(
    create_model(route = "iv", data = test_data, drop_input = "NOSUCHCOL"),
    "not found in dataset"
  )
})

test_that("create_model marks specified column as DROP in $INPUT", {
  test_data <- data.frame(
    ID = 1L, TIME = c(0, 1, 2), DV = c(0, 10, 5), CMT = 1L,
    AMT = c(100, 0, 0), EVID = c(1L, 0L, 0L), MDV = c(1L, 0L, 0L), BW = 70, HT = 180
  )
  mod <- create_model(route = "iv", data = test_data, drop_input = c("BW", "HT"), verbose = FALSE)

  input_line <- get_input_line(mod$code)
  expect_match(input_line, "DROP", fixed = TRUE)
  expect_match(input_line, "DROP", fixed = TRUE)
  expect_false(grepl("(?<![=A-Za-z0-9_])BW(?!=)", input_line, perl = TRUE))
  expect_false(grepl("(?<![=A-Za-z0-9_])HT(?!=)", input_line, perl = TRUE))
})
