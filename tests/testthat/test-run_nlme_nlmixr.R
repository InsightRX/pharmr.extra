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

# ── PRED via a zero-random-effects solve (#136) ────────────────────────────

test_that("shape_rxsolve_output: uses the supplied population prediction for PRED", {
  raw <- data.frame(id = c(1, 2), time = c(1, 1), ipredSim = c(7, 9), sim = c(7.1, 9.1))
  src <- data.frame(ID = c(1, 2), TIME = c(1, 1), EVID = c(0, 0), MDV = c(0, 0))
  out <- shape_rxsolve_output(raw, src, pop_pred = c(8, 8))
  expect_equal(out$PRED, c(8, 8))
  expect_equal(out$IPRED, c(7, 9))
})

test_that("shape_rxsolve_output: recycles the population prediction across replicates", {
  ## rxSolve stacks `nsim` contiguous replicates in identical row order, so a
  ## single-replicate PRED vector applies to each block in turn.
  raw <- data.frame(
    sim.id = c(1, 1, 2, 2),
    id = c(1, 2, 1, 2), time = c(1, 1, 1, 1),
    ipredSim = c(7, 9, 6, 10), sim = c(7.1, 9.1, 6.1, 10.1)
  )
  src <- data.frame(ID = c(1, 2), TIME = c(1, 1), EVID = c(0, 0), MDV = c(0, 0))
  out <- shape_rxsolve_output(raw, src, pop_pred = c(8, 12))
  expect_equal(out$PRED, c(8, 12, 8, 12))
})

test_that("shape_rxsolve_output: falls back to IPRED when pop_pred doesn't fit", {
  raw <- data.frame(id = c(1, 2), time = c(1, 1), ipredSim = c(7, 9), sim = c(7.1, 9.1))
  src <- data.frame(ID = c(1, 2), TIME = c(1, 1), EVID = c(0, 0), MDV = c(0, 0))
  ## length 3 doesn't divide 2 rows
  expect_equal(shape_rxsolve_output(raw, src, pop_pred = c(1, 2, 3))$PRED, c(7, 9))
  expect_equal(shape_rxsolve_output(raw, src, pop_pred = numeric(0))$PRED, c(7, 9))
  expect_equal(shape_rxsolve_output(raw, src, pop_pred = NULL)$PRED, c(7, 9))
})

test_that("solve_population_prediction: returns NULL when rxode2 has no zeroRe()", {
  local_mocked_bindings(rx_zero_re_supported = function() FALSE, .package = "pharmr.extra")
  expect_null(solve_population_prediction(function() NULL, data.frame(ID = 1, TIME = 0)))
})

test_that("solve_population_prediction: returns NULL when the zeroed solve fails", {
  skip_if_not_installed("rxode2")
  ## Not a model function, so rxSolve errors — should degrade, not propagate.
  expect_null(solve_population_prediction("not a model", data.frame(ID = 1, TIME = 0)))
})

# ── create_vpc_data(): nlmixr2 argument handling (#136) ────────────────────

test_that("create_vpc_data_nlmixr: obs and sim come from the same dataset", {
  ## `data` wins over anything attached to the model, and is what the
  ## simulation is handed — the two must not resolve a dataset independently.
  d <- data.frame(
    ID = rep(1:2, each = 3),
    TIME = rep(c(0, 1, 2), 2),
    DV = c(0, 5, 4, 0, 6, 3),
    EVID = rep(c(1, 0, 0), 2),
    MDV = rep(c(1, 0, 0), 2),
    AMT = rep(c(100, 0, 0), 2)
  )
  seen <- NULL
  local_mocked_bindings(
    run_sim_nlmixr = function(data, n_iterations, ...) {
      seen <<- list(data = data, n = n_iterations, args = list(...))
      obs_rows <- data[data$EVID == 0, c("ID", "TIME"), drop = FALSE]
      out <- obs_rows[rep(seq_len(nrow(obs_rows)), n_iterations), , drop = FALSE]
      out$DV <- seq_len(nrow(out))
      out
    },
    .package = "pharmr.extra"
  )
  out <- create_vpc_data_nlmixr(model = "unused", data = d, n = 3, verbose = FALSE)
  expect_equal(seen$data, d)
  expect_equal(nrow(out$obs), 4)
  expect_equal(nrow(out$sim), 12)
  expect_equal(nrow(out$sim) %% nrow(out$obs), 0)
})

test_that("create_vpc_data_nlmixr: draws a seed per call and forwards an explicit one", {
  d <- data.frame(ID = 1, TIME = c(0, 1), DV = c(0, 5), EVID = c(1, 0),
                  MDV = c(1, 0), AMT = c(100, 0))
  seeds <- c()
  local_mocked_bindings(
    run_sim_nlmixr = function(data, n_iterations, seed, ...) {
      seeds <<- c(seeds, seed)
      data.frame(ID = 1, TIME = 1, DV = 1)
    },
    .package = "pharmr.extra"
  )
  create_vpc_data_nlmixr(model = "unused", data = d, n = 1, verbose = FALSE)
  create_vpc_data_nlmixr(model = "unused", data = d, n = 1, verbose = FALSE)
  expect_length(seeds, 2)
  expect_false(identical(seeds[1], seeds[2]))

  create_vpc_data_nlmixr(model = "unused", data = d, n = 1, seed = 99, verbose = FALSE)
  expect_equal(seeds[3], 99)
})

test_that("create_vpc_data_nlmixr: selects obs by EVID when MDV doesn't line up", {
  ## Observation rows flagged MDV = 1 (BLQ handling and the like) are still
  ## solved by rxSolve, so obs has to follow EVID instead.
  d <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 5, 0),
    EVID = c(1, 0, 0), MDV = c(1, 0, 1), AMT = c(100, 0, 0)
  )
  local_mocked_bindings(
    run_sim_nlmixr = function(data, n_iterations, ...) {
      data.frame(ID = 1, TIME = rep(c(1, 2), n_iterations), DV = 1)
    },
    .package = "pharmr.extra"
  )
  out <- create_vpc_data_nlmixr(model = "unused", data = d, n = 2, verbose = FALSE)
  expect_equal(nrow(out$obs), 2)
  expect_equal(out$obs$TIME, c(1, 2))
})

test_that("create_vpc_data_nlmixr: aborts when obs and sim cannot be aligned", {
  d <- data.frame(ID = 1, TIME = c(0, 1, 2), DV = c(0, 5, 6),
                  EVID = c(1, 0, 0), MDV = c(1, 0, 0), AMT = c(100, 0, 0))
  local_mocked_bindings(
    run_sim_nlmixr = function(...) data.frame(ID = 1, TIME = 1:5, DV = 1),
    .package = "pharmr.extra"
  )
  expect_error(
    create_vpc_data_nlmixr(model = "unused", data = d, n = 2, verbose = FALSE),
    "don't line up"
  )
})

test_that("create_vpc_data: forwards data/seed and warns on NONMEM-only args", {
  ## Only the dispatch is exercised, so a stand-in carrying the pharmpy class
  ## is enough — no pharmpy or nlmixr2 install needed to check the routing.
  mod_nl <- structure(list(), class = c("pharmpy.model.model.Model", "list"))

  seen <- NULL
  local_mocked_bindings(
    get_tool_from_model = function(model) "nlmixr",
    create_vpc_data_nlmixr = function(...) {
      seen <<- list(...)
      "DISPATCHED"
    },
    .package = "pharmr.extra"
  )
  d <- data.frame(ID = 1, TIME = 0, DV = 0)
  out <- create_vpc_data(model = mod_nl, data = d, n = 7, seed = 5, verbose = FALSE)
  expect_equal(out, "DISPATCHED")
  expect_equal(seen$data, d)
  expect_equal(seen$seed, 5)
  expect_equal(seen$n, 7)

  expect_warning(
    create_vpc_data(model = mod_nl, id = "run1", verbose = FALSE),
    "NONMEM-only"
  )
  expect_silent(create_vpc_data(model = mod_nl, verbose = FALSE))
})
