# sample_uncertainty_parameters(): input handling (#155).
#
# The draws themselves are checked against real fits in test-run_sim.R and
# test-run_sim-nwpri.R; these tests cover how the estimates and covariance
# matrix are normalised before they reach Pharmpy. They need Pharmpy (pandas
# is imported up front) but not NONMEM.

.pheno_inputs <- function() {
  list(
    model = pharmr::load_example_model("pheno"),
    res   = pharmr::load_example_modelfit_results("pheno")
  )
}

test_that("sample_uncertainty_parameters needs named parameter estimates", {
  skip_if_pharmpy_not_available()
  expect_error(
    sample_uncertainty_parameters(
      model = NULL, parameter_estimates = c(1, 2),
      covariance_matrix = diag(2), n = 2, seed = 1
    ),
    "must be a named vector"
  )
})

test_that("sample_uncertainty_parameters needs a covariance matrix with names", {
  skip_if_pharmpy_not_available()
  expect_error(
    sample_uncertainty_parameters(
      model = NULL, parameter_estimates = c(A = 1, B = 2),
      covariance_matrix = diag(2), n = 2, seed = 1
    ),
    "parameter names as row/column names"
  )
})

test_that("sample_uncertainty_parameters names covariance parameters without estimates", {
  skip_if_pharmpy_not_available()
  cov <- diag(2)
  dimnames(cov) <- list(c("A", "C"), c("A", "C"))
  expect_error(
    sample_uncertainty_parameters(
      model = NULL, parameter_estimates = c(A = 1, B = 2),
      covariance_matrix = cov, n = 2, seed = 1
    ),
    "Missing: C"
  )
})

test_that("sample_uncertainty_parameters takes R and pandas inputs alike", {
  skip_if_pharmpy_not_available()
  p <- .pheno_inputs()
  pe  <- p$res$parameter_estimates
  cov <- as.matrix(p$res$covariance_matrix)

  ref <- sample_uncertainty_parameters(p$model, pe, cov, n = 3, seed = 5)
  expect_s3_class(ref, "data.frame")
  expect_equal(dim(ref), c(3L, ncol(cov)))
  expect_equal(names(ref), colnames(cov))
  ## A plain R data.frame, without the pandas index py_to_r leaves behind.
  expect_null(attr(ref, "pandas.index"))
  expect_equal(rownames(ref), as.character(1:3))

  ## The same inputs handed over as pandas objects.
  pd <- reticulate::import("pandas", convert = FALSE)
  pe_py  <- pd$Series(as.list(pe))
  cov_py <- reticulate::r_to_py(as.data.frame(cov))
  cov_py$index <- as.list(colnames(cov))
  expect_equal(
    sample_uncertainty_parameters(p$model, pe_py, cov_py, n = 3, seed = 5),
    ref
  )

  ## A covariance matrix that lost its row names is read off its columns.
  cov_unnamed <- cov
  rownames(cov_unnamed) <- NULL
  expect_equal(
    sample_uncertainty_parameters(p$model, pe, cov_unnamed, n = 3, seed = 5),
    ref
  )

  ## Estimates in another order, or with extra parameters the covariance does
  ## not span, line up with the covariance by name.
  pe_extra <- c(rev(pe), EXTRA = 1)
  expect_equal(
    sample_uncertainty_parameters(p$model, pe_extra, cov, n = 3, seed = 5),
    ref
  )
})
