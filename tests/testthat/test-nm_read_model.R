# TODO: add example NONMEM model file to package, then add tests for modelfile argument

test_that("parses simple NONMEM code with as_block = FALSE", {
  code <- "$PROBLEM Test Model\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  out <- nm_read_model(code = code, as_block = FALSE)
  
  expect_named(out, c("PROBLEM", "INPUT", "PK", "THETA", "ESTIMATION"))
  expect_equal(out$PROBLEM, "$PROBLEM Test Model")
  expect_equal(out$INPUT, "$INPUT ID TIME DV")
  expect_equal(out$PK, c("$PK", "CL = THETA(1)"))
  expect_equal(out$THETA, "$THETA (0, 5)")
  expect_equal(out$ESTIMATION, "$ESTIMATION METHOD=1")
  expect_s3_class(out, "NONMEM")
  expect_s3_class(out, "list")
})

test_that("parses NONMEM code with as_block = TRUE", {
  code <- "$PROBLEM Test Model\n$INPUT ID TIME DV\n$PK\nCL = THETA(1)\nV = THETA(2)\n$THETA (0, 5)\n$ESTIMATION METHOD=1"
  out <- nm_read_model(code = code, as_block = TRUE)
  
  expect_equal(out$PROBLEM, "$PROBLEM Test Model")
  expect_equal(out$INPUT, "$INPUT ID TIME DV")
  expect_equal(out$PK, "$PK\nCL = THETA(1)\nV = THETA(2)")
  expect_equal(out$THETA, "$THETA (0, 5)")
  expect_equal(out$ESTIMATION, "$ESTIMATION METHOD=1")
})

test_that("trims leading whitespace from lines", {
  code <- "  $PROBLEM Test Model\n\t$INPUT ID TIME DV\n  $PK\nCL = THETA(1)\n$THETA (0, 5)"
  out <- nm_read_model(code = code, as_block = FALSE)
  
  expect_equal(out$PROBLEM, "$PROBLEM Test Model")
  expect_equal(out$INPUT, "$INPUT ID TIME DV")
  expect_equal(out$PK, c("$PK", "CL = THETA(1)"))
  expect_equal(out$THETA, "$THETA (0, 5)")
})

test_that("handles multiple blocks of same type", {
  code <- "$PROBLEM Test\n$TABLE FILE=sdtab001\n$TABLE FILE=patab001\n$ESTIMATION METHOD=1"
  out <- nm_read_model(code = code, as_block = FALSE)
  
  expect_equal(length(out$TABLE), 2)
  expect_equal(out$TABLE[1], "$TABLE FILE=sdtab001")
  expect_equal(out$TABLE[2], "$TABLE FILE=patab001")
})

test_that("handles model with multiple sections", {
  code <- "$PROBLEM PK Model\n$INPUT ID TIME DV AMT EVID\n$DATA data.csv\n$PK\nCL = THETA(1)\nV = THETA(2)\n$ERROR\nY = F + F*EPS(1)\n$THETA (0, 5)\n(0, 10)\n$OMEGA 0.1\n$SIGMA 0.1\n$ESTIMATION METHOD=1"
  out <- nm_read_model(code = code, as_block = FALSE)
  
  expect_named(out, c("PROBLEM", "INPUT", "DATA", "PK", "ERROR", "THETA", "OMEGA", "SIGMA", "ESTIMATION"))
  expect_equal(out$PROBLEM, "$PROBLEM PK Model")
  expect_equal(out$INPUT, "$INPUT ID TIME DV AMT EVID")
  expect_equal(out$DATA, "$DATA data.csv")
  expect_equal(out$PK, c("$PK", "CL = THETA(1)", "V = THETA(2)"))
  expect_equal(out$ERROR, c("$ERROR", "Y = F + F*EPS(1)"))
  expect_equal(out$THETA, c("$THETA (0, 5)", "(0, 10)"))
  expect_equal(out$OMEGA, "$OMEGA 0.1")
  expect_equal(out$SIGMA, "$SIGMA 0.1")
  expect_equal(out$ESTIMATION, "$ESTIMATION METHOD=1")
})

test_that("handles model with multiple sections with as_block = TRUE", {
  code <- "$PROBLEM PK Model\n$INPUT ID TIME DV AMT EVID\n$DATA data.csv\n$PK\nCL = THETA(1)\nV = THETA(2)\n$ERROR\nY = F + F*EPS(1)\n$THETA (0, 5)\n(0, 10)\n$OMEGA 0.1\n$SIGMA 0.1\n$ESTIMATION METHOD=1"
  out <- nm_read_model(code = code, as_block = TRUE)
  
  expect_equal(out$PK, "$PK\nCL = THETA(1)\nV = THETA(2)")
  expect_equal(out$ERROR, "$ERROR\nY = F + F*EPS(1)")
  expect_equal(out$THETA, "$THETA (0, 5)\n(0, 10)")
})

test_that("handles empty lines between blocks", {
  # TODO: Is this desirable behaviour, or would it be better to strip out the
  # empty "" lines afterwards?
  code <- "$PROBLEM Test\n\n$INPUT ID TIME DV\n\n$PK\nCL = THETA(1)"
  out <- nm_read_model(code = code, as_block = FALSE)
  
  expect_equal(out$PROBLEM, c("$PROBLEM Test", ""))
  expect_equal(out$INPUT, c("$INPUT ID TIME DV", ""))
})

test_that("errors when no code blocks detected", {
  code <- "This is not a NONMEM model\nJust some text\nNo dollar signs"
  
  expect_error(
    out <- nm_read_model(code = code),
    "Sorry, no code blocks detected in NONMEM file."
  )
})
