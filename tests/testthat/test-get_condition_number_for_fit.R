# get_condition_number_for_fit() ------------------------------------------------
test_that("calculates condition number for valid fit with correlation matrix", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  out <- get_condition_number_for_fit(fit)
  
  # Should return a numeric value (not NA)
  expect_type(out, "double")
  expect_false(is.na(out))
  expect_true(out > 0) # Condition number should always be positive
  expect_equal(out, 21.342945)
})

test_that("returns NA when correlation_matrix is NULL", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  fit_no_corr <- fit
  fit_no_corr$correlation_matrix <- NULL
  out <- get_condition_number_for_fit(fit_no_corr)
  
  expect_true(is.na(out))
})

test_that("returns NA when correlation matrix is not square", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  fit_non_square <- fit
  fit_non_square$correlation_matrix <- matrix(1:6, nrow = 2, ncol = 3)
  out <- get_condition_number_for_fit(fit_non_square)
  
  expect_true(is.na(out))
})

test_that("handles edge case of 1x1 matrix", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  fit_1x1 <- fit
  fit_1x1$correlation_matrix <- matrix(1)
  out <- get_condition_number_for_fit(fit_1x1)
  
  # 1x1 matrix should have condition number of 1 (max/min of single eigenvalue):
  expect_equal(out, 1)
})

test_that("handles identity matrix correctly", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  fit_identity <- fit
  fit_identity$correlation_matrix <- diag(5)
  out <- get_condition_number_for_fit(fit_identity)
  
  # Identity matrix should have condition number of 1:
  expect_equal(out, 1)
})

test_that("works with correlation matrix from data.frame", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  fit_df <- fit
  # Convert to data.frame, should still work after as.matrix():
  fit_df$correlation_matrix <- as.data.frame(fit$correlation_matrix)
  out <- get_condition_number_for_fit(fit_df)
  
  expect_type(out, "double")
  expect_false(is.na(out))
  expect_true(out > 0)
  expect_equal(out, 21.342945)
})

# calc_condition_number() -------------------------------------------------------
test_that("calculates correctly for square matrix", {
  local_pharmr.extra_options()
  # Identity matrix:
  mat <- diag(5)
  expect_equal(calc_condition_number(mat), 1)
  
  # Simple 2x2 matrix:
  mat <- matrix(c(2, 1, 1, 2), nrow = 2)
  # Eigenvalues of this matrix: 3 and 1; Condition number = 3/1 = 3.
  expect_equal(calc_condition_number(mat), 3, tolerance = 1e-10)
})

test_that("handles positive definite matrix", {
  local_pharmr.extra_options()
  mat <- matrix(c(2, 0.5, 0.5, 2), nrow = 2)
  out <- calc_condition_number(mat)
  
  expect_type(out, "double")
  expect_true(out >= 1) # Condition number should be >= 1
  expect_true(is.finite(out))
})
