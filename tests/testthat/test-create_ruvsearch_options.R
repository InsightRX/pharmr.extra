test_that("default options are returned correctly", {
  out <- create_ruvsearch_options()
  expect_type(out, "list")
  expect_equal(out$groups, 4L)
  expect_equal(out$p_value, 0.001)
  expect_equal(out$max_iter, 3L)
  expect_null(out$skip)
  expect_null(out$dv)
  expect_null(out$strictness)
  expect_null(out$parameter_uncertainty_method)
})

test_that("skip with single valid value works", {
  out <- create_ruvsearch_options(skip = "IIV_on_RUV")
  expect_equal(out$skip, "IIV_on_RUV")
})

test_that("skip with multiple valid values works", {
  out <- create_ruvsearch_options(skip = c("power", "combined"))
  expect_equal(out$skip, c("power", "combined"))
})

test_that("all valid skip values are accepted", {
  valid <- c("IIV_on_RUV", "power", "combined", "time_varying", "additive")
  out <- create_ruvsearch_options(skip = valid)
  expect_equal(out$skip, valid)
})

test_that("NULL skip is not included in output list", {
  out <- create_ruvsearch_options(skip = NULL)
  expect_false("skip" %in% names(out))
})

test_that("invalid skip value raises error", {
  expect_error(
    create_ruvsearch_options(skip = "INVALID"),
    "Invalid"
  )
})

test_that("mix of valid and invalid skip raises error", {
  expect_error(
    create_ruvsearch_options(skip = c("power", "INVALID")),
    "Invalid"
  )
})

test_that("groups and max_iter are coerced to integer", {
  out <- create_ruvsearch_options(groups = 6, max_iter = 5)
  expect_identical(out$groups, 6L)
  expect_identical(out$max_iter, 5L)
})

test_that("p_value is passed through", {
  out <- create_ruvsearch_options(p_value = 0.05)
  expect_equal(out$p_value, 0.05)
})

test_that("dv is coerced to integer and included", {
  out <- create_ruvsearch_options(dv = 2)
  expect_identical(out$dv, 2L)
})

test_that("NULL dv is not included in output list", {
  out <- create_ruvsearch_options(dv = NULL)
  expect_false("dv" %in% names(out))
})

test_that("strictness is included when provided", {
  out <- create_ruvsearch_options(strictness = "minimization_successful")
  expect_equal(out$strictness, "minimization_successful")
})

test_that("NULL strictness is not included in output list", {
  out <- create_ruvsearch_options(strictness = NULL)
  expect_false("strictness" %in% names(out))
})

test_that("valid parameter_uncertainty_method is accepted", {
  for (m in c("SANDWICH", "SMAT", "RMAT", "EFIM")) {
    out <- create_ruvsearch_options(parameter_uncertainty_method = m)
    expect_equal(out$parameter_uncertainty_method, m)
  }
})

test_that("invalid parameter_uncertainty_method raises error", {
  expect_error(
    create_ruvsearch_options(parameter_uncertainty_method = "INVALID"),
    "Invalid"
  )
})

test_that("NULL parameter_uncertainty_method is not included in output list", {
  out <- create_ruvsearch_options(parameter_uncertainty_method = NULL)
  expect_false("parameter_uncertainty_method" %in% names(out))
})

test_that("full custom options list is built correctly", {
  out <- create_ruvsearch_options(
    skip = c("additive", "time_varying"),
    groups = 6L,
    p_value = 0.01,
    max_iter = 2L,
    dv = 1L,
    strictness = "minimization_successful",
    parameter_uncertainty_method = "SANDWICH"
  )
  expect_equal(out$skip, c("additive", "time_varying"))
  expect_equal(out$groups, 6L)
  expect_equal(out$p_value, 0.01)
  expect_equal(out$max_iter, 2L)
  expect_equal(out$dv, 1L)
  expect_equal(out$strictness, "minimization_successful")
  expect_equal(out$parameter_uncertainty_method, "SANDWICH")
})
