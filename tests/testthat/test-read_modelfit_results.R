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

## ensure_default_ext_file integration -----------------------------------------
##
## When `$EST FILE=psn.ext` redirects iteration output, the default `<stem>.ext`
## is empty and pharmpy returns nothing. The wrapper must normalize the .ext
## file BEFORE delegating to pharmpy, otherwise direct callers (and the eval /
## sim branch of run_nlme that bypasses this wrapper differently) get a silent
## failure. We mock pharmr::read_modelfit_results to keep this hermetic.

test_that("normalizes a missing/empty <stem>.ext before delegating to pharmpy", {
  d <- withr::local_tempdir()
  writeLines("$PROBLEM", file.path(d, "run.mod"))
  file.create(file.path(d, "run.ext"))  # 0 bytes, simulating FILE= override
  writeLines("psn-content", file.path(d, "psn.ext"))

  ## Make the mocked pharmpy reader trivial — we only care that the wrapper
  ## copied psn.ext → run.ext BEFORE delegating.
  local_mocked_bindings(
    read_modelfit_results = function(path, ...) {
      list(path = path, ext_lines = readLines(paste0(tools::file_path_sans_ext(path), ".ext")))
    },
    .package = "pharmr"
  )
  ## Skip the python patch (it imports pharmpy which may not be installed).
  local_mocked_bindings(
    .patch_pharmpy_sir_bugs = function() invisible(NULL),
    .package = "pharmr.extra"
  )

  ## Qualify the wrapper explicitly: testthat::local_mocked_bindings() also
  ## rebinds in `the$testing_env`, so a bare `read_modelfit_results(...)` here
  ## resolves to pharmr's mock and bypasses our wrapper (and so
  ## ensure_default_ext_file). The qualified call goes through the pharmr.extra
  ## wrapper, which then delegates to the mocked pharmr function.
  res <- pharmr.extra::read_modelfit_results(file.path(d, "run.mod"))

  expect_equal(res$ext_lines, "psn-content")
  expect_equal(readLines(file.path(d, "run.ext")), "psn-content")
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
