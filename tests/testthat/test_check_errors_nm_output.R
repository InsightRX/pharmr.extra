test_that("check_errors_nm_output handles different outputs correctly", {
  expect_error(check_errors_nm_output(c("This is fine", "DATA ERROR: Issue found", "Details here")), 
               "DATA ERROR: Issue found")
  expect_error(check_errors_nm_output(c("AN ERROR WAS FOUND at line 10", "Please check")), 
               "AN ERROR WAS FOUND at line 10")
  expect_silent(check_errors_nm_output(c("All good", "No errors")))
  expect_error(check_errors_nm_output(c("DATA ERROR in the data", "AN ERROR WAS FOUND in processing")), 
               "DATA ERROR in the data")
})