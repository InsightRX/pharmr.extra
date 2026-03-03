## has_covariance_record() --------------------------------------------------

test_that("has_covariance_record() returns TRUE for model with $COV", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  expect_true(has_covariance_record(mod))
})

test_that("has_covariance_record() returns FALSE for model without $COV", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  expect_false(has_covariance_record(mod))
})

## get_covariance_record() --------------------------------------------------

test_that("get_covariance_record() returns the $COV record content", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  cov <- get_covariance_record(mod)
  expect_type(cov, "character")
  expect_true(grepl("\\$COV", cov))
})

test_that("get_covariance_record() errors for non-NONMEM model", {
  local_pharmr.extra_options()
  expect_error(
    get_covariance_record(list()),
    "not a Pharmpy NONMEM model"
  )
})

## update_covariance_record() -----------------------------------------------

test_that("update_covariance_record() updates the $COV record", {
  local_pharmr.extra_options()
  mod <- make_model_with_cov()
  new_record <- "$COV UNCOND PRINT=E"
  mod_updated <- update_covariance_record(mod, new_record)
  expect_s3_class(mod_updated, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("PRINT=E", mod_updated$code))
})

test_that("update_covariance_record() errors when no $COV record exists", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  expect_error(
    update_covariance_record(mod, "$COV UNCOND"),
    "\\$COVARIANCE"
  )
})
