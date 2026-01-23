test_that("basic covariance matrix creation works", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, nonmem = FALSE)
  
  expect_equal(class(out), c("matrix", "array"))
  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 2)
  expect_equal(rownames(out), c("CL", "V"))
  expect_equal(colnames(out), c("CL", "V"))
})

test_that("diagonal elements are variances (SD^2)", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0)
  out <- get_cov_matrix(pars, nonmem = FALSE)
  
  expect_equal(out["CL", "CL"], 0.1^2)
  expect_equal(out["V", "V"], 0.2^2)
})

test_that("off-diagonal elements are covariances (rho * SD1 * SD2)", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, nonmem = FALSE)
  
  expected_cov <- 0.3 * 0.1 * 0.2
  expect_equal(out["CL", "V"], expected_cov)
  expect_equal(out["V", "CL"], expected_cov) # Symmetric
})

test_that("covariance matrix is symmetric", {
  pars <- list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, 
                 "CL~V" = 0.3, "CL~KA" = 0.5, "V~KA" = 0.2)
  out <- get_cov_matrix(pars, nonmem = FALSE)
  
  expect_true(isSymmetric(out))
})

test_that("keep_all = FALSE keeps only parameters with correlations", {
  pars <- list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, keep_all = FALSE, nonmem = FALSE)
  
  expect_equal(nrow(out), 2)
  expect_equal(rownames(out), c("CL", "V"))
  expect_false("KA" %in% rownames(out))
})

test_that("keep_all = TRUE keeps all parameters", {
  pars <- list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, keep_all = TRUE, nonmem = FALSE)
  
  expect_equal(nrow(out), 3)
  expect_true(all(c("CL", "V", "KA") %in% rownames(out)))
})

test_that("triangle = TRUE returns lower triangle as vector", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, triangle = TRUE, nonmem = FALSE)
  
  expect_type(out, "double")
  expect_length(out, 3) # 2 diagonal + 1 off-diagonal
  expect_true(all(out >= 0))
})

test_that("nonmem = TRUE returns formatted character vector", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0.3)
  out <- get_cov_matrix(pars, nonmem = TRUE)
  
  expect_type(out, "character")
  expect_length(out, 2) # One line per row
  # First line should have one value (diagonal)
  expect_length(strsplit(out[1], "\\s+")[[1]], 1)
  # Second line should have two values (diagonal and off-diagonal)
  expect_length(strsplit(out[2], "\\s+")[[1]], 2)
})

test_that("limit parameter enforces minimum covariance values", {
  pars <- list("CL" = 0.1, "V" = 0.2, "CL~V" = 0.001)  # Very small correlation
  out <- get_cov_matrix(pars, nonmem = FALSE, limit = 0.001)
  
  expect_gte(out["CL", "V"], 0.001)
  expect_gte(out["V", "CL"], 0.001)
})

test_that("limit parameter affects all off-diagonal elements", {
  pars <- list("CL" = 0.001, "V" = 0.001, "CL~V" = 0.0001)
  out <- get_cov_matrix(pars, nonmem = FALSE, limit = 0.005)
  
  # Even though correlation is small, limit should be applied
  expect_gte(out["CL", "V"], 0.005)
  expect_gte(out["V", "CL"], 0.005)
})

test_that("limit = NULL disables limit enforcement", {
  pars <- list("CL" = 0.001, "V" = 0.001, "CL~V" = 0.0001)
  out <- get_cov_matrix(pars, nonmem = FALSE, limit = NULL)
  
  expected_cov <- 0.0001 * 0.001 * 0.001
  expect_equal(out["CL", "V"], expected_cov)
})

test_that("works with multiple correlations", {
  pars <- list(
    "CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3, "CL~KA" = 0.5, "V~KA" = 0.2
  )
  out <- get_cov_matrix(pars, nonmem = FALSE)
  
  expect_equal(out["CL", "V"], 0.3 * 0.1 * 0.2)
  expect_equal(out["CL", "KA"], 0.5 * 0.1 * 0.3)
  expect_equal(out["V", "KA"], 0.2 * 0.2 * 0.3)
  expect_true(isSymmetric(out))
})
