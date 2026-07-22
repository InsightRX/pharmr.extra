lst_path <- test_path("fixtures", "run_with_ext", "run.lst")

test_that("parse_lst extracts the control stream", {
  p <- parse_lst(lst_path)
  expect_type(p$control_stream, "character")
  expect_length(p$control_stream, 1)
  expect_match(p$control_stream, "\\$PROBLEM")
  expect_match(p$control_stream, "\\$THETA")
  # the NM-TRAN echo and everything after it must be excluded
  expect_no_match(p$control_stream, "NM-TRAN MESSAGES")
})

test_that("parse_lst recovers final THETA estimates (rounded) from the .lst", {
  p <- parse_lst(lst_path)
  # gold values live in the sibling run.ext final row (-1000000000):
  # THETA 1.32434, 27.9381, 181.119
  expect_equal(p$theta, c(1.32, 27.9, 181), tolerance = 1e-6)
})

test_that("parse_lst recovers OMEGA and SIGMA covariance matrices", {
  p <- parse_lst(lst_path)
  expect_equal(dim(p$omega), c(2L, 2L))
  # OMEGA(1,1)=0.136164, OMEGA(2,1)=0, OMEGA(2,2)=0.187579
  expect_equal(p$omega, matrix(c(0.136, 0, 0, 0.188), 2, 2), tolerance = 1e-6)
  expect_equal(dim(p$sigma), c(1L, 1L))
  expect_equal(p$sigma[1, 1], 0.117, tolerance = 1e-6)
})

test_that("parse_lst reads the full-precision OFV", {
  p <- parse_lst(lst_path)
  expect_equal(p$ofv, 1248.4264065825578, tolerance = 1e-10)
})

test_that("parse_lst accepts contents via `code`", {
  code <- readChar(lst_path, file.info(lst_path)$size)
  p <- parse_lst(code = code)
  expect_equal(p$theta, c(1.32, 27.9, 181), tolerance = 1e-6)
})

test_that("parse_lst errors on a report without a final-estimate section", {
  code <- "$PROBLEM x\n$THETA 1\n\nNM-TRAN MESSAGES\n AN ERROR WAS FOUND\n"
  expect_error(parse_lst(code = code), "FINAL PARAMETER ESTIMATE")
})

test_that("parse_lst errors when neither input is supplied", {
  expect_error(parse_lst(), "NONMEM report file")
})

test_that("parse_lst uses the last FINAL section for multi-step estimation", {
  # SAEM+IMP runs print one FINAL PARAMETER ESTIMATE section per step; the
  # last one holds the final estimates (matching the last .ext table).
  code <- paste(
    "$PROBLEM x", "$THETA 1", "",
    "NM-TRAN MESSAGES", "",
    " FINAL PARAMETER ESTIMATE",
    " THETA - VECTOR OF FIXED EFFECTS PARAMETERS", "         TH 1", "",
    "         1.00E+00",
    " STANDARD ERROR OF ESTIMATE",
    " THETA - VECTOR OF FIXED EFFECTS PARAMETERS", "         TH 1", "",
    "         9.00E-01",
    " FINAL PARAMETER ESTIMATE",
    " THETA - VECTOR OF FIXED EFFECTS PARAMETERS", "         TH 1", "",
    "         2.00E+00",
    sep = "\n"
  )
  expect_equal(parse_lst(code = code)$theta, 2.0)
})

test_that("parse_lst reassembles matrix rows wrapped onto continuation lines", {
  # NONMEM wraps rows wider than 12 elements onto a continuation line with no
  # `+` prefix. Here the second OMEGA row is split to exercise that folding.
  code <- paste(
    "$PROBLEM x", "$THETA 1", "", "NM-TRAN MESSAGES", "",
    " FINAL PARAMETER ESTIMATE",
    " OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",
    "         ETA1      ETA2", "",
    " ETA1", "+        1.00E-01", "",
    " ETA2", "+        2.00E-02", "          3.00E-01", "",
    sep = "\n"
  )
  om <- parse_lst(code = code)$omega
  expect_equal(dim(om), c(2L, 2L))
  expect_equal(om, matrix(c(0.1, 0.02, 0.02, 0.30), 2, 2), tolerance = 1e-9)
})

test_that(".nm_numbers maps structural-zero dots to 0", {
  expect_equal(
    pharmr.extra:::.nm_numbers("+       .........  2.50E-05"),
    c(0, 2.5e-05)
  )
})

test_that("parse_lst folds an all-dots continuation line into the matrix row", {
  # A wide row can wrap a segment that is entirely structural-zero dots (no
  # E-notation); it must still be folded so later columns don't shift left.
  code <- paste(
    "$PROBLEM x", "$THETA 1", "", "NM-TRAN MESSAGES", "",
    " FINAL PARAMETER ESTIMATE",
    " OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",
    "         ETA1      ETA2", "",
    " ETA1", "+        1.00E-01", "",
    " ETA2", "+        .........", "          2.00E-01", "",
    sep = "\n"
  )
  om <- parse_lst(code = code)$omega
  expect_equal(om, matrix(c(0.1, 0, 0, 0.2), 2, 2), tolerance = 1e-9)
})

test_that("parse_lst OFV fallback reads #OBJV, not the #OBJT banner", {
  # When the WITHOUT CONSTANT line is absent, the value lives on #OBJV; the
  # `MINIMUM VALUE OF OBJECTIVE FUNCTION` phrase sits on the number-less #OBJT.
  code <- paste(
    "$PROBLEM x", "$THETA 1", "", "NM-TRAN MESSAGES", "",
    " FINAL PARAMETER ESTIMATE",
    " THETA - VECTOR OF FIXED EFFECTS PARAMETERS", "         TH 1", "",
    "         1.00E+00",
    " #OBJT:****   MINIMUM VALUE OF OBJECTIVE FUNCTION   ****",
    " #OBJV:****            5335.951            ****",
    sep = "\n"
  )
  expect_equal(parse_lst(code = code)$ofv, 5335.951, tolerance = 1e-9)
})

# End-to-end: needs NONMEM/Pharmpy to build the model and apply estimates.
test_that("create_model_from_lst applies the recovered estimates", {
  skip_if_nonmem_not_available()
  model <- create_model_from_lst(lst_path, verbose = FALSE)
  expect_true(inherits(model, "pharmpy.model.model.Model"))
  inits <- model$parameters$inits
  # THETA2 = POP_CL in this model; final (rounded) value is 27.9
  expect_equal(unname(inits[["POP_CL"]]), 27.9, tolerance = 1e-6)
})

test_that("create_model_from_lst writes model code when save_as is given", {
  skip_if_nonmem_not_available()
  out <- withr::local_tempfile(fileext = ".mod")
  model <- create_model_from_lst(lst_path, save_as = out, verbose = FALSE)
  expect_true(file.exists(out))
  expect_match(paste(readLines(out), collapse = "\n"), "\\$THETA")
})
