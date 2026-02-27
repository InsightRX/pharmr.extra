test_that("parse_estimation_options handles flat list for single method", {
  result <- parse_estimation_options("foce", list(MAXEVAL = 100))
  expect_length(result, 1)
  expect_equal(result[[1]], list(MAXEVAL = 100))
})

test_that("parse_estimation_options handles NULL options", {
  result <- parse_estimation_options("foce", NULL)
  expect_length(result, 1)
  expect_null(result[[1]])
})

test_that("parse_estimation_options handles empty list for multiple methods", {
  result <- parse_estimation_options(c("saem", "imp"), list())
  expect_length(result, 2)
  expect_equal(result[[1]], list())
  expect_equal(result[[2]], list())
})

test_that("parse_estimation_options warns with flat list and multiple methods", {
  expect_warning(
    result <- parse_estimation_options(c("saem", "imp"), list(NBURN = 200)),
    "flat list"
  )
  expect_length(result, 2)
  expect_equal(result[[1]], list(NBURN = 200))
  expect_equal(result[[2]], list())
})

test_that("parse_estimation_options handles named list of lists", {
  result <- parse_estimation_options(
    c("saem", "imp"),
    list(SAEM = list(NBURN = 200), IMP = list(NITER = 5))
  )
  expect_length(result, 2)
  expect_equal(result[[1]], list(NBURN = 200))
  expect_equal(result[[2]], list(NITER = 5))
})

test_that("parse_estimation_options is case-insensitive for method names in nested list", {
  result <- parse_estimation_options(
    c("SAEM", "IMP"),
    list(saem = list(NBURN = 200), imp = list(NITER = 5))
  )
  expect_equal(result[[1]], list(NBURN = 200))
  expect_equal(result[[2]], list(NITER = 5))
})

test_that("parse_estimation_options allows partial spec in nested list", {
  result <- parse_estimation_options(
    c("saem", "imp"),
    list(SAEM = list(NBURN = 200))
  )
  expect_equal(result[[1]], list(NBURN = 200))
  expect_equal(result[[2]], list())
})

test_that("parse_estimation_options errors if nested list with single method", {
  expect_error(
    parse_estimation_options("foce", list(FOCE = list(MAXEVAL = 100))),
    "list of lists"
  )
})

test_that("parse_estimation_options errors on unknown method names in nested list", {
  expect_error(
    parse_estimation_options(
      c("saem", "imp"),
      list(SAEM = list(NBURN = 200), BOGUS = list())
    ),
    "Unknown method"
  )
})

test_that("get_estimation_options returns correct defaults and handles user options", {
  # Test NONMEM FOCE defaults
  defaults_nonmem_foce <- get_estimation_options("nonmem", "foce", NULL)
  expect_equal(
    defaults_nonmem_foce,
    list(
      MAXEVAL = "2000",
      PRINT = "5",
      POSTHOC = "",
      NOABORT = ""
    )
  )
  
  # Test NONMEM SAEM defaults
  defaults_nonmem_saem <- get_estimation_options("nonmem", "saem", NULL)
  expect_equal(
    defaults_nonmem_saem,
    list(
      NBURN = "500",
      NITER = "1000",
      ISAMPLE = "2"
    )
  )
  
  # Test user-specified options override defaults
  user_opts <- list(MAXEVAL = 1000, PRINT = 1)
  custom_opts <- get_estimation_options("nonmem", "foce", user_opts)
  expect_equal(
    custom_opts,
    list(
      MAXEVAL = "1000",
      PRINT = "1",
      POSTHOC = "",
      NOABORT = ""
    )
  )
  
  # Test nlmixr returns empty list (as per current implementation)
  nlmixr_opts <- get_estimation_options("nlmixr", "foce", NULL)
  expect_equal(nlmixr_opts, list())
}) 