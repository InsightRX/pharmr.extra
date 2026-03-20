test_that("parse_data_dictionary returns full defaults when given empty list", {
  result <- parse_data_dictionary(list())
  expect_equal(result$ID, "ID")
  expect_equal(result$TIME, "TIME")
  expect_equal(result$DV, "DV")
  expect_equal(result$EVID, "EVID")
  expect_equal(result$AMT, "AMT")
  expect_equal(result$CMT, "CMT")
  expect_equal(result$MDV, "MDV")
})

test_that("parse_data_dictionary overrides specified keys", {
  result <- parse_data_dictionary(list(ID = "SUBJID", DV = "CONC"))
  expect_equal(result$ID, "SUBJID")
  expect_equal(result$DV, "CONC")
  # Non-overridden keys keep defaults
  expect_equal(result$TIME, "TIME")
  expect_equal(result$EVID, "EVID")
})

test_that("parse_data_dictionary keeps all default keys present", {
  result <- parse_data_dictionary(list(ID = "SUBJID"))
  expect_setequal(names(result), c("ID", "TIME", "DV", "EVID", "AMT", "CMT", "MDV"))
})

test_that("parse_data_dictionary adds extra keys beyond defaults", {
  result <- parse_data_dictionary(list(DOSE = "DOSE_MG"))
  expect_equal(result$DOSE, "DOSE_MG")
  expect_equal(result$ID, "ID")
})

test_that("parse_data_dictionary accepts custom default", {
  result <- parse_data_dictionary(list(), default = list(ID = "USUBJID"))
  expect_equal(result$ID, "USUBJID")
  expect_null(result$TIME)
})
