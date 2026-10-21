## Minimal stand-in for a Pharmpy bootstrap results object: only the
## `parameter_estimates` field is used by plot_bootstrap().
mock_bootstrap_results <- function(n = 50) {
  set.seed(1)
  list(
    parameter_estimates = data.frame(
      POP_CL = rnorm(n, 4.5, 0.3),
      POP_V = rnorm(n, 32, 2),
      IIV_CL = rnorm(n, 0.09, 0.01)
    )
  )
}

test_that("plot_bootstrap returns a faceted ggplot of all parameters", {
  p <- plot_bootstrap(mock_bootstrap_results())
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
  expect_setequal(unique(p$data$parameter), c("POP_CL", "POP_V", "IIV_CL"))
  expect_equal(nrow(p$data), 150)
  ## histogram + density + CI lines (default ci = 0.95)
  expect_length(p$layers, 3)
})

test_that("plot_bootstrap draws CI lines at the requested quantiles", {
  bs <- mock_bootstrap_results()
  p <- plot_bootstrap(bs, ci = 0.9)
  ci_data <- p$layers[[3]]$data
  expect_equal(nrow(ci_data), 6)
  cl <- ci_data$value[ci_data$parameter == "POP_CL"]
  expect_equal(
    unname(cl),
    unname(stats::quantile(bs$parameter_estimates$POP_CL, c(0.05, 0.95)))
  )
})

test_that("plot_bootstrap omits CI lines when ci = NULL", {
  p <- plot_bootstrap(mock_bootstrap_results(), ci = NULL)
  expect_length(p$layers, 2)
})

test_that("plot_bootstrap overlays original estimates, ignoring unknown names", {
  p <- plot_bootstrap(
    mock_bootstrap_results(),
    original_estimates = c(POP_CL = 4.5, POP_V = 32, NOT_A_PARAM = 1),
    ci = NULL
  )
  expect_length(p$layers, 3)
  orig_data <- p$layers[[3]]$data
  expect_equal(orig_data$parameter, c("POP_CL", "POP_V"))
  expect_equal(orig_data$value, c(4.5, 32))
})

test_that("plot_bootstrap skips the overlay when no original estimate matches", {
  p <- plot_bootstrap(
    mock_bootstrap_results(),
    original_estimates = c(NOT_A_PARAM = 1),
    ci = NULL
  )
  expect_length(p$layers, 2)
})

test_that("plot_bootstrap subsets parameters and warns about missing ones", {
  p <- plot_bootstrap(mock_bootstrap_results(), parameters = "POP_CL")
  expect_equal(unique(p$data$parameter), "POP_CL")

  expect_warning(
    p <- plot_bootstrap(mock_bootstrap_results(), parameters = c("POP_V", "FOO")),
    "FOO"
  )
  expect_equal(unique(p$data$parameter), "POP_V")
})

test_that("plot_bootstrap aborts when none of the requested parameters exist", {
  expect_error(
    suppressWarnings(plot_bootstrap(mock_bootstrap_results(), parameters = "FOO")),
    "None of the requested"
  )
})

test_that("plot_bootstrap aborts on empty parameter estimates", {
  expect_error(
    plot_bootstrap(list(parameter_estimates = data.frame(POP_CL = numeric(0)))),
    "no rows"
  )
})

test_that("plot_bootstrap aborts when parameter estimates cannot be extracted", {
  ## a function cannot be coerced to a data.frame
  bad <- list(parameter_estimates = function() NULL)
  expect_error(plot_bootstrap(bad), "Could not extract")
})
