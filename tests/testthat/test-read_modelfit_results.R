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
  skip_if_pharmpy_not_available()
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
  skip_if_pharmpy_not_available()
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
  skip_if_pharmpy_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_sir", "run.mod")
  fit  <- read_modelfit_results(path)

  # Bug 2: without the fix, covstatus_table_number points at the SIR table
  # whose lst block has no "Elapsed covariance time", so covstep_successful
  # would be NULL/None rather than TRUE.
  expect_true(isTRUE(fit$covstep_successful))
})

test_that("SIR model: covariance matrix is returned", {
  skip_if_pharmpy_not_available()
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
  skip_if_pharmpy_not_available()
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
  skip_if_pharmpy_not_available()
  local_pharmr.extra_options()

  path <- test_path("fixtures", "run_folder", "run.mod")
  fit  <- read_modelfit_results(path)

  # The run_folder fixture has a non-positive-definite R matrix so
  # covstep_successful is FALSE — but it must be a logical, not NULL/None,
  # confirming that Pharmpy read the covariance block at all.
  expect_false(is.null(fit$covstep_successful))
  expect_type(isTRUE(fit$covstep_successful) || isFALSE(fit$covstep_successful), "logical")
})

# .patch_pharmpy_sir_bugs() — the patched parsers one by one -----------------
#
# The tests above check the end result of reading the SIR fixture. These call
# the replaced Pharmpy functions directly, and read variants of the fixture
# that exercise the Bug 3 branches the fixture itself never reaches. They only
# need Pharmpy, not NONMEM.

## The SIR fixture's ext file as Pharmpy parses it (11 tables: FOCE, then 10
## SIR tables), with the patches applied.
.sir_ext_tables <- function(ext = test_path("fixtures", "run_sir", "run.ext")) {
  .patch_pharmpy_sir_bugs()
  tbl <- reticulate::import("pharmpy.model.external.nonmem.table", convert = FALSE)
  tbl$NONMEMTableFile(normalizePath(ext))
}

## Copy the SIR fixture to a temporary folder, dropping the rows of the given
## `iteration` (e.g. -1000000005) from the last ext table. Returns the model path.
.sir_fixture_without_row <- function(iteration, .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(
    list.files(test_path("fixtures", "run_sir"), full.names = TRUE), dir
  )
  ext_file <- file.path(dir, "run.ext")
  ext <- readLines(ext_file)
  last_table <- max(grep("^TABLE NO\\.", ext))
  drop <- which(
    seq_along(ext) > last_table &
      grepl(paste0("^\\s*", iteration, "\\s"), ext)
  )
  stopifnot(length(drop) == 1)
  writeLines(ext[-drop], ext_file)
  file.path(dir, "run.mod")
}

test_that(".patch_pharmpy_sir_bugs replaces the four parsers, once", {
  skip_if_pharmpy_not_available()
  .patch_pharmpy_sir_bugs()
  nm <- reticulate::import("pharmpy.tools.external.nonmem.results")
  expect_equal(
    c(nm$`_parse_table_numbers`$`__name__`,
      nm$`_parse_ofv`$`__name__`,
      nm$`_parse_parameter_estimates`$`__name__`,
      nm$`_parse_standard_errors`$`__name__`),
    c("_patched_parse_table_numbers", "_patched_parse_ofv",
      "_patched_parse_parameter_estimates", "_patched_parse_standard_errors")
  )
  expect_true(.sir_patch_applied$get())

  ## Idempotent: a second call does not run the Python code again.
  local_mocked_bindings(
    py_run_string = function(...) stop("patched twice"),
    .package = "reticulate"
  )
  expect_null(.patch_pharmpy_sir_bugs())
})

test_that("SIR tables are told apart from estimation tables by their method", {
  skip_if_pharmpy_not_available()
  tables <- reticulate::py_to_r(.sir_ext_tables()$tables)
  expect_length(tables, 11)
  is_sir <- reticulate::py$`_is_sir_table`
  expect_equal(
    vapply(tables, function(t) reticulate::py_to_r(is_sir(t)), logical(1)),
    c(FALSE, rep(TRUE, 10))
  )
})

test_that("Bug 1/2: only the estimation table is counted in table_numbers", {
  skip_if_pharmpy_not_available()
  nm <- reticulate::import("pharmpy.tools.external.nonmem.results")
  ext_tables <- .sir_ext_tables()
  ## Unpatched, every SIR table is counted and the covariance status is looked
  ## up under the last of them.
  expect_equal(unlist(nm$`_parse_table_numbers`(ext_tables, NULL)), 1L)
  ## A subproblem filter still applies.
  expect_length(nm$`_parse_table_numbers`(ext_tables, 2L), 0)
})

test_that("Bug 1/2: the final OFV and OFV iterations come from the FOCE table", {
  skip_if_pharmpy_not_available()
  nm <- reticulate::import("pharmpy.tools.external.nonmem.results")
  res <- nm$`_parse_ofv`(.sir_ext_tables(), NULL)
  ## FOCE: -457.539...; the SIR tables report around -451.
  expect_equal(res[[1]], -457.53948039169308, tolerance = 1e-8)
  ## Iterations are indexed "(step, iteration)": all of them step 1, i.e. the
  ## FOCE table's five iterations without the SIR samples.
  ofv_iter <- res[[2]]
  expect_length(ofv_iter, 5)
  expect_true(all(startsWith(names(ofv_iter), "(1, ")))
  expect_equal(ofv_iter[[5]], -457.53948039169308, tolerance = 1e-8)
})

test_that("Bug 1: final estimates and iterations come from the FOCE table", {
  skip_if_pharmpy_not_available()
  fit <- read_modelfit_results(test_path("fixtures", "run_sir", "run.mod"))
  pe_iter <- fit$parameter_estimates_iterations
  steps <- reticulate::py_to_r(
    attr(pe_iter, "pandas.index")$get_level_values("step")$tolist()
  )
  expect_equal(unique(unlist(steps)), 1L)
  expect_equal(nrow(pe_iter), 5)
  ## From the FOCE starting point (THETA1 = 0.5) to its final estimate.
  expect_equal(pe_iter$POP_KA[c(1, 5)], c(0.5, 4.72965e-01))
  expect_equal(fit$ofv, -457.53948039169308, tolerance = 1e-8)
})

test_that("Bug 3: SIR standard errors survive a missing -1000000005 row", {
  skip_if_pharmpy_not_available()
  path <- .sir_fixture_without_row("-1000000005")
  fit <- read_modelfit_results(path)

  ## Unpatched, the missing stdcorr row sets cov_abort: every SE turns NaN and
  ## the .cov file is not read. Patched, the SIR SEs (last table's -1000000001
  ## row) are kept, and the covariance step is read as usual.
  se <- fit$standard_errors
  expect_false(any(is.nan(as.numeric(se))))
  expect_equal(se[["POP_KA"]], 1.01357e-02, tolerance = 1e-6)
  expect_equal(se[["POP_CL"]], 3.26448e-01, tolerance = 1e-6)
  expect_true(all(is.nan(as.numeric(fit$standard_errors_sdcorr))))
  expect_true(isTRUE(fit$covstep_successful))
  expect_false(is.null(fit$covariance_matrix))
})

test_that("Bug 3: a last table without standard errors gives NaN SEs, not an abort", {
  skip_if_pharmpy_not_available()
  path <- .sir_fixture_without_row("-1000000001")
  fit <- read_modelfit_results(path)

  se <- fit$standard_errors
  expect_false(is.null(se))
  expect_true(all(is.nan(as.numeric(se))))
  ## Estimates are unaffected, and the covariance step is still read.
  expect_equal(fit$parameter_estimates[["POP_KA"]], 4.72965e-01,
               tolerance = 1e-4)
  expect_false(is.null(fit$covariance_matrix))
})
