# Tests for the nlmixr2 dispatch path of run_nlme() / run_sim() / create_vpc_data().
# These exercise unit-level helpers and the dispatch surface; the integration
# tests that actually fit a model are slow and gated on nlmixr2 availability.

skip_if_no_nlmixr2 <- function() {
  testthat::skip_if_not_installed("nlmixr2")
  testthat::skip_if_not_installed("rxode2")
}

# ── extract_nlmixr_function() ──────────────────────────────────────────────

test_that("extract_nlmixr_function: pulls the model function from pharmpy code", {
  code <- paste(
    "my_mod <- function() {",
    "  ini({ POP_CL <- c(0, 1, Inf) })",
    "  model({ d/dt(A) = -POP_CL*A; A ~ prop(0.1) })",
    "}",
    "fit <- nlmixr2(my_mod, dataset, est = 'focei')",
    sep = "\n"
  )
  fn <- extract_nlmixr_function(code)
  expect_true(is.function(fn))
})

test_that("extract_nlmixr_function: returns NULL when no function assignment", {
  fn <- extract_nlmixr_function("x <- 1\ny <- x + 1")
  expect_null(fn)
})

test_that("extract_nlmixr_function: returns NULL on un-parseable code", {
  expect_null(extract_nlmixr_function("not valid R syntax }{["))
})

# ── expand_predictions_to_full_dataset() ───────────────────────────────────

test_that("expand_predictions_to_full_dataset: inserts NA at non-obs rows (EVID)", {
  obs_df <- data.frame(PRED = c(10, 5, 2.5), IPRED = c(11, 4.8, 2.4))
  src <- data.frame(
    ID   = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 4),
    EVID = c(1, 0, 0, 0)
  )
  out <- expand_predictions_to_full_dataset(obs_df, src)
  expect_equal(nrow(out), nrow(src))
  expect_true(all(is.na(out[1, ])))
  expect_equal(out$PRED[2:4], c(10, 5, 2.5))
  expect_equal(out$IPRED[2:4], c(11, 4.8, 2.4))
})

test_that("expand_predictions_to_full_dataset: prefers MDV over EVID", {
  obs_df <- data.frame(PRED = c(7, 3))
  src <- data.frame(
    ID   = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),  # both EVID==0
    MDV  = c(1, 0, 0)   # MDV agrees here, but presence routes through MDV branch
  )
  out <- expand_predictions_to_full_dataset(obs_df, src)
  expect_equal(out$PRED, c(NA_real_, 7, 3))
})

test_that("expand_predictions_to_full_dataset: falls back to obs-only on row-count mismatch", {
  obs_df <- data.frame(PRED = c(10, 5))  # only 2 rows
  src <- data.frame(
    ID   = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 4),
    EVID = c(1, 0, 0, 0)             # 3 obs rows
  )
  out <- expand_predictions_to_full_dataset(obs_df, src)
  expect_identical(out, obs_df)
})

test_that("expand_predictions_to_full_dataset: passes through when input_data is NULL", {
  obs_df <- data.frame(PRED = c(10, 5))
  expect_identical(expand_predictions_to_full_dataset(obs_df, NULL), obs_df)
})

test_that("expand_predictions_to_full_dataset: passes through when no EVID/MDV", {
  obs_df <- data.frame(PRED = c(10, 5))
  src <- data.frame(ID = c(1, 1), TIME = c(0, 1))
  expect_identical(expand_predictions_to_full_dataset(obs_df, src), obs_df)
})

# ── shape_rxsolve_output() ─────────────────────────────────────────────────

test_that("shape_rxsolve_output: drops duplicate ipredSim when IPRED already present", {
  raw <- data.frame(
    id = c(1, 1), time = c(0, 1),
    IPRED = c(10, 5), ipredSim = c(99, 99),
    sim = c(11, 5.2)
  )
  src <- data.frame(ID = c(1, 1), TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0), MDV = c(1, 0))
  out <- shape_rxsolve_output(raw, src)
  expect_false("ipredSim" %in% names(out))
  expect_true(all(out$IPRED == c(10, 5)))
})

test_that("shape_rxsolve_output: renames id/time/sim and synthesises PRED", {
  raw <- data.frame(id = 1, time = 0, ipredSim = 7, sim = 8)
  src <- data.frame(ID = 1, TIME = 0, EVID = 0, MDV = 0)
  out <- shape_rxsolve_output(raw, src)
  expect_true(all(c("ID", "TIME", "IPRED", "DV", "PRED") %in% names(out)))
  expect_equal(out$DV, 8)
  expect_equal(out$IPRED, 7)
  expect_equal(out$PRED, 7)  # PRED is reported as IPRED for nlmixr2
})

test_that("shape_rxsolve_output: carries EVID/AMT/MDV from input dataset", {
  raw <- data.frame(id = c(1, 1), time = c(0, 1), ipredSim = c(7, 8), sim = c(7.1, 8.1))
  src <- data.frame(
    ID = c(1, 1, 1), TIME = c(0, 0, 1),
    EVID = c(1, 0, 0), MDV = c(1, 0, 0), AMT = c(100, 0, 0)
  )
  out <- shape_rxsolve_output(raw, src)
  expect_equal(out$EVID, c(0, 0))   # rxSolve emits obs rows; obs match wins
  expect_equal(out$AMT, c(0, 0))
})

# ── run_nlme dispatch ──────────────────────────────────────────────────────

test_that("run_nlme: routes nlmixr2 models to run_nlme_nlmixr()", {
  skip_if_no_nlmixr2()
  mod <- pharmr::create_basic_pk_model(administration = "iv")
  mod <- pharmr::convert_model(mod, "nonmem")
  mod_nl <- pharmr::convert_model(mod, "nlmixr")
  expect_equal(get_tool_from_model(mod_nl), "nlmixr")

  ## Mock the inner fitter so the test stays fast and doesn't actually fit.
  local_mocked_bindings(
    run_nlme_nlmixr = function(...) "DISPATCHED",
    .package = "pharmr.extra"
  )
  withr::local_dir(tempdir())
  out <- run_nlme(model = mod_nl, id = "dispatch_test", verbose = FALSE)
  expect_equal(out, "DISPATCHED")
})

# ── run_sim dispatch ───────────────────────────────────────────────────────

test_that("run_sim: routes nlmixr2 models to run_sim_nlmixr()", {
  skip_if_no_nlmixr2()
  mod <- pharmr::create_basic_pk_model(administration = "iv")
  mod <- pharmr::convert_model(mod, "nonmem")
  mod_nl <- pharmr::convert_model(mod, "nlmixr")

  local_mocked_bindings(
    run_sim_nlmixr = function(...) "DISPATCHED",
    .package = "pharmr.extra"
  )
  withr::local_dir(tempdir())
  out <- run_sim(model = mod_nl)
  expect_equal(out, "DISPATCHED")
})

# ── get_advan: returns NULL for non-NONMEM ─────────────────────────────────

test_that("get_advan: returns invisible NULL for nlmixr models", {
  skip_if_no_nlmixr2()
  mod <- pharmr::create_basic_pk_model(administration = "iv")
  mod <- pharmr::convert_model(mod, "nonmem")
  mod_nl <- pharmr::convert_model(mod, "nlmixr")
  expect_null(get_advan(mod_nl))
})
