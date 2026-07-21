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

test_that(".nm_numbers maps structural-zero dots to 0", {
  expect_equal(.nm_numbers("+       .........  2.50E-05"), c(0, 2.5e-05))
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
