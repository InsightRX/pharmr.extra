base_model_code <- paste0(
  "$PROBLEM Test\n",
  "$INPUT ID TIME DV AMT EVID MDV CONC\n",
  "$DATA data.csv IGNORE=@\n",
  "$SUBROUTINES ADVAN1 TRANS2\n",
  "$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
  "$ERROR\nY=F+EPS(1)\n",
  "$THETA (0,10) ; POP_CL\n",
  "$THETA (0,50) ; POP_V\n",
  "$SIGMA 0.1\n",
  "$EST METHOD=1\n"
)

# Input validation ----

test_that("set_dv errors when dv is not a character", {
  model <- pharmr::read_model_from_string(base_model_code)
  expect_error(set_dv(model, 123), "`dv` must be a single character string")
})

test_that("set_dv errors when dv has length > 1", {
  model <- pharmr::read_model_from_string(base_model_code)
  expect_error(set_dv(model, c("DV", "CONC")), "`dv` must be a single character string")
})

test_that("set_dv errors when column does not exist in datainfo", {
  model <- pharmr::read_model_from_string(base_model_code)
  expect_error(set_dv(model, "NOSUCHCOL"), "not found in datainfo")
})

# Core behaviour ----

test_that("set_dv returns the same model unchanged when column is already DV", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "DV")
  expect_equal(result$datainfo$dv_column$name, "DV")
})

test_that("set_dv sets the new column as DV", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  expect_equal(result$datainfo$dv_column$name, "CONC")
})

test_that("set_dv demotes the old DV column: 'DV' is DROPped from $INPUT", {
  # When old DV is the reserved name 'DV', it is DROPped entirely from $INPUT
  # (NONMEM's reserved 'DV' cannot be kept as an ordinary passthrough column).
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  col_names <- result$datainfo$names
  expect_false("DV" %in% col_names)
})

test_that("set_dv returns a valid Pharmpy model object", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  expect_true(inherits(result, "pharmpy.model.model.Model"))
})


test_that("set_dv does not modify other column types", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  # ID, TIME, AMT, EVID, MDV should be unchanged
  for (col_name in c("ID", "TIME", "AMT")) {
    before <- model$datainfo[[col_name]]$type
    after  <- result$datainfo[[col_name]]$type
    expect_equal(after, before, info = paste("type of", col_name, "should not change"))
  }
})

# $INPUT record update ----

test_that("set_dv updates $INPUT record: new column becomes DV=col", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  input_pairs <- result$internals$control_stream$get_records("INPUT")[[1]]$option_pairs
  # CONC should now be mapped as DV=CONC
  expect_equal(input_pairs[["DV"]], "CONC")
})

test_that("set_dv updates $INPUT record: old DV column is DROPped", {
  model <- pharmr::read_model_from_string(base_model_code)
  result <- set_dv(model, "CONC")
  input_pairs <- result$internals$control_stream$get_records("INPUT")[[1]]$option_pairs
  # Old DV (reserved name) should be DROPped
  expect_true("DROP" %in% names(input_pairs))
  expect_false("DV" %in% names(input_pairs) && is.null(input_pairs[["DV"]]))
})
