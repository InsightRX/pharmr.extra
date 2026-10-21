## run_simulation() hands the solve to rxode2::rxSolve(); mock it so the tests
## need no compiled nlmixr2 model. The fake returns a constant concentration of
## `dose / 10` for every subject at every sampling time.
mock_fit_obj <- function(fixef = c(tcl = 1, prop_sd = 0.1, add_sd = 0.5)) {
  list(omega = diag(c(0.1, 0.2)), fixef = fixef)
}

local_mock_rxsolve <- function(.local_envir = parent.frame()) {
  env <- new.env()
  env$calls <- list()
  local_mocked_bindings(
    rxSolve = function(object, omega, events, nsim, ...) {
      env$calls <- c(env$calls, list(list(omega = omega, events = events, nsim = nsim, dots = list(...))))
      obs <- as.data.frame(events)
      times <- obs$time[is.na(obs$amt)]
      dose <- obs$amt[!is.na(obs$amt)][1]
      data.frame(
        sim.id = rep(seq_len(nsim), each = length(times)),
        time = rep(times, nsim),
        ipredSim = dose / 10
      )
    },
    .package = "rxode2",
    .env = .local_envir
  )
  env
}

test_that("run_simulation returns aggregated summaries per scenario", {
  mock <- local_mock_rxsolve()
  res <- run_simulation(
    mock_fit_obj(),
    dose = c(100, 200),
    interval = c(12, 24),
    n_days = 2,
    n_subjects = 5,
    res_error = FALSE
  )
  expect_length(mock$calls, 2)
  expect_equal(mock$calls[[1]]$nsim, 5)
  expect_equal(mock$calls[[1]]$omega, diag(c(0.1, 0.2)))
  expect_false(mock$calls[[1]]$dots$simVariability)
  expect_true(all(c("mean", "median", "q_5", "q_95", "sd") %in% names(res)))
  expect_setequal(
    unique(res$scenario),
    c("Scenario 1: 100 mg / 12 hrs", "Scenario 2: 200 mg / 24 hrs")
  )
  s1 <- res[res$scenario == "Scenario 1: 100 mg / 12 hrs", ]
  expect_true(all(s1$mean == 10))
  expect_true(all(s1$sd == 0))
  expect_true(all(s1$dose == 100))
  expect_true(all(s1$interval == 12))
  ## n_days = 2 with q12h dosing -> 4 doses, observations every 2h until (4+1)*12
  expect_equal(range(s1$time), c(0, 60))
})

test_that("run_simulation returns per-subject data when aggregate = FALSE", {
  local_mock_rxsolve()
  res <- run_simulation(
    mock_fit_obj(),
    dose = 100,
    interval = 24,
    n_days = 1,
    n_subjects = 3,
    aggregate = FALSE,
    res_error = FALSE
  )
  expect_true(all(c("id", "time", "y", "scenario", "dose", "interval") %in% names(res)))
  expect_setequal(unique(res$id), 1:3)
  expect_true(all(res$y == 10))
})

test_that("run_simulation uses n_doses when n_days is NULL", {
  local_mock_rxsolve()
  res <- run_simulation(
    mock_fit_obj(),
    dose = 100,
    interval = 12,
    n_doses = 2,
    n_days = NULL,
    n_subjects = 2,
    aggregate = FALSE,
    res_error = FALSE
  )
  expect_equal(max(res$time), 36)
})

test_that("run_simulation shrinks omega when bsv = FALSE", {
  mock <- local_mock_rxsolve()
  run_simulation(
    mock_fit_obj(), dose = 100, interval = 24, n_days = 1,
    n_subjects = 2, bsv = FALSE, res_error = FALSE
  )
  expect_equal(mock$calls[[1]]$omega, diag(c(0.1, 0.2)) * 1e-6)
})

test_that("run_simulation adds residual error from the fit's error parameters", {
  local_mock_rxsolve()
  res <- run_simulation(
    mock_fit_obj(), dose = 100, interval = 24, n_days = 1,
    n_subjects = 20, aggregate = FALSE, res_error = TRUE
  )
  expect_false(all(res$y == 10))

  ## no error parameters in the fit -> no residual error added
  res <- run_simulation(
    mock_fit_obj(fixef = c(tcl = 1)), dose = 100, interval = 24, n_days = 1,
    n_subjects = 5, aggregate = FALSE, res_error = TRUE
  )
  expect_true(all(res$y == 10))
})

test_that("run_simulation saves the output when path is given", {
  local_mock_rxsolve()
  path <- withr::local_tempfile(fileext = ".rds")
  res <- run_simulation(
    mock_fit_obj(), dose = 100, interval = 24, n_days = 1,
    n_subjects = 2, res_error = FALSE, path = path
  )
  expect_equal(readRDS(path), res)
})

test_that("run_simulation validates its inputs", {
  expect_error(
    run_simulation(mock_fit_obj(), dose = c(100, 200), interval = 12),
    "Length of `dose`"
  )
  expect_error(
    run_simulation(mock_fit_obj(), dose = 100, interval = 12, n_doses = NULL, n_days = NULL),
    "Either `n_doses` or `n_days`"
  )
})
