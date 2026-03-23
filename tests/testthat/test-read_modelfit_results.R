# read_modelfit_results() — FOCE + SIR covariance step -------------------------
#
# These tests use a real NONMEM run fixture (`run_sir/`) that was produced with
# $COVARIANCE ... SIRSAMPLE=500, creating a two-table ext file.  They verify
# the three Pharmpy bugs patched in read_modelfit_results.R:
#
#   Bug 1/2 — skipping the SIR ext table so parameter estimates and
#             covstatus_table_number come from the FOCE table only.
#   Bug 3   — keeping valid standard errors when the SIR ext table lacks the
#             -1000000005 row (omega/sigma SE in stdcorr form).

test_that("SIR model: parameter estimates are non-NaN and match FOCE values", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_sir", "run.mod")
  fit  <- read_modelfit_results(path)

  pe <- fit$parameter_estimates
  expect_false(is.null(pe))
  expect_false(all(is.nan(as.numeric(pe))))

  # FOCE final estimates from the ext file (-1000000000 row):
  #   THETA1 = 4.72965E-01, THETA2 = 1.09121E+01, THETA3 = 9.26065E+01
  pe_vals <- as.numeric(pe)
  pe_names <- names(pe)

  expect_true("POP_KA" %in% pe_names)
  expect_true("POP_CL" %in% pe_names)
  expect_true("POP_V"  %in% pe_names)

  expect_equal(pe[["POP_KA"]], 4.72965e-01, tolerance = 1e-4)
  expect_equal(pe[["POP_CL"]], 1.09121e+01, tolerance = 1e-3)
  expect_equal(pe[["POP_V"]],  9.26065e+01, tolerance = 1e-2)
})

test_that("SIR model: standard errors are non-NaN", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_sir", "run.mod")
  fit  <- read_modelfit_results(path)

  # Bug 3: without the fix, cov_abort=True overrides SEs with NaN.
  # SEs come from the last SIR table's -1000000001 row (SIR-based SEs,
  # different from the FOCE SEs in table 1).
  se <- fit$standard_errors
  expect_false(is.null(se))
  expect_true("POP_KA" %in% names(se))
  expect_false(any(is.nan(as.numeric(se))))
  expect_true(all(as.numeric(se[c("POP_KA", "POP_CL", "POP_V")]) > 0))
})

test_that("SIR model: covariance step is reported as successful", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_sir", "run.mod")
  fit  <- read_modelfit_results(path)

  # Bug 2: without the fix, covstatus_table_number points at the SIR table
  # whose lst block has no "Elapsed covariance time", so covstep_successful
  # would be NULL/None rather than TRUE.
  expect_true(isTRUE(fit$covstep_successful))
})

test_that("SIR model: covariance matrix is returned", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_sir", "run.mod")
  fit  <- read_modelfit_results(path)

  # Without the fix the .cov file is never read, so covariance_matrix is NULL.
  cov <- fit$covariance_matrix
  expect_false(is.null(cov))
  # Should be a square matrix with one row/col per estimated parameter.
  cov_mat <- as.matrix(cov)
  expect_equal(nrow(cov_mat), ncol(cov_mat))
  expect_gt(nrow(cov_mat), 0)
})

# Regression: FOCE-only model (no SIR) still works after the patches ----------

test_that("FOCE-only model: parameter estimates are unaffected by SIR patches", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_folder", "run.mod")
  fit  <- read_modelfit_results(path)

  pe <- fit$parameter_estimates
  expect_false(is.null(pe))
  expect_false(all(is.nan(as.numeric(pe))))
  # Sanity-check sign: all thetas should be positive for this model.
  theta_names <- names(pe)[grepl("^POP_", names(pe))]
  expect_true(all(pe[theta_names] > 0))
})

test_that("FOCE-only model: covstep_successful is not NULL after reading", {
  skip_if_nonmem_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_folder", "run.mod")
  fit  <- read_modelfit_results(path)

  # The run_folder fixture has a non-positive-definite R matrix so
  # covstep_successful is FALSE — but it must be a logical, not NULL/None,
  # confirming that Pharmpy read the covariance block at all.
  expect_false(is.null(fit$covstep_successful))
  expect_type(isTRUE(fit$covstep_successful) || isFALSE(fit$covstep_successful), "logical")
})
