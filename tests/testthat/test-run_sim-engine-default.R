# Default uncertainty engine and worker-init resilience (#134) -----------------
#
# `uncertainty_engine = "auto"` (the default) prefers NWPRI, which benchmarks
# far faster than the replicate loop, and falls back to `"replicates"` wherever
# NWPRI cannot be used. Naming an engine explicitly must still error rather
# than fall back, so an explicit request is never silently overridden.

test_that("resolve_uncertainty_engine picks nwpri only where it applies", {
  expect_equal(resolve_uncertainty_engine("nonmem", 1, verbose = FALSE), "nwpri")
  ## NWPRI is a NONMEM feature
  expect_equal(resolve_uncertainty_engine("nlmixr2", 1, verbose = FALSE),
               "replicates")
  ## every NWPRI subproblem redraws the parameters, so it cannot repeat a draw
  expect_equal(resolve_uncertainty_engine("nonmem", 2, verbose = FALSE),
               "replicates")
})

test_that("resolve_uncertainty_engine says which engine it picked", {
  expect_message(resolve_uncertainty_engine("nonmem", 1), "nwpri")
  ## and why, when it could not use NWPRI
  expect_message(resolve_uncertainty_engine("nlmixr2", 1), "NONMEM feature")
  expect_message(resolve_uncertainty_engine("nonmem", 3), "n_iterations")
  expect_silent(resolve_uncertainty_engine("nonmem", 1, verbose = FALSE))
})

## The NWPRI engine returns a named list of tables per regimen, simulation
## table first; this stands in for one.
.stub_nwpri_tables <- function(n_uncertainty = 2L) {
  tab <- do.call(rbind, lapply(seq_len(n_uncertainty), function(k) {
    data.frame(ID = 1L, TIME = c(0, 6, 12), DV = c(0, 5, 3),
               EVID = c(1L, 0L, 0L), .uncertainty = k)
  }))
  list(simtab = tab)
}

## Bindings that stub out everything needing a real NONMEM/pharmpy install, so
## the tests only observe *which branch ran*. Returned rather than applied, so
## each test can install them in its own frame (this testthat has no
## `.local_envir` argument).
.nwpri_stub_bindings <- function(used) {
  list(
    get_nmfe_location = function(...) "/nonexistent/nmfe",
    add_nwpri_prior = function(model, fit, ...) model,
    run_nwpri_regimen_tables = function(...) {
      used$engine <- "nwpri"
      .stub_nwpri_tables()
    },
    run_nlme = function(...) {
      used$engine <- "replicates"
      .mock_nlme_result()
    },
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    .package = "pharmr.extra"
  )
}

.fake_fit <- function() {
  list(parameter_estimates = c(POP_CL = 1, POP_V = 10),
       covariance_matrix = diag(2))
}

test_that("run_sim (nonmem): the default engine is now nwpri", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  used <- new.env()
  do.call(local_mocked_bindings, .nwpri_stub_bindings(used))
  local_mocked_bindings(set_initial_estimates = function(model, inits) model,
                        .package = "pharmr")
  out <- run_sim(
    fit = .fake_fit(), model = make_model_without_cov(), data = .sim_dat(),
    n_uncertainty = 2, verbose = FALSE
  )
  expect_equal(used$engine, "nwpri")
  expect_equal(sort(unique(out$.uncertainty)), 1:2)
})

test_that("run_sim: the default falls back to replicates where NWPRI cannot run", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  ## `n_iterations > 1` used to be perfectly ordinary under the old default, so
  ## the new default must fall back rather than start aborting those calls.
  used <- new.env()
  do.call(local_mocked_bindings, .nwpri_stub_bindings(used))
  local_mocked_bindings(set_initial_estimates = function(model, inits) model,
                        .package = "pharmr")
  out <- run_sim(
    fit = .fake_fit(), model = make_model_without_cov(), data = .sim_dat(),
    n_uncertainty = 2, n_iterations = 2, verbose = FALSE
  )
  expect_equal(used$engine, "replicates")
  expect_equal(sort(unique(out$.uncertainty)), 1:2)
})

test_that("run_sim: naming an engine still errors instead of falling back", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  args <- list(
    fit = .fake_fit(),
    model = make_model_without_cov(), data = .sim_dat(),
    n_uncertainty = 2, uncertainty_engine = "nwpri", verbose = FALSE
  )
  expect_error(do.call(run_sim, c(args, list(n_iterations = 2))),
               "requires .*n_iterations = 1")
  expect_error(do.call(run_sim, c(args, list(tool = "nlmixr2"))),
               "NONMEM feature")
})

test_that("run_sim: a point-estimate run is unaffected by the engine default", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  used <- new.env()
  do.call(local_mocked_bindings, .nwpri_stub_bindings(used))
  local_mocked_bindings(set_initial_estimates = function(model, inits) model,
                        .package = "pharmr")
  ## No `n_uncertainty`, so there is no uncertainty to propagate and NWPRI must
  ## not be dragged in just because it is the default engine.
  out <- run_sim(model = make_model_without_cov(), data = .sim_dat(),
                 verbose = FALSE)
  expect_equal(used$engine, "replicates")
  expect_false(".uncertainty" %in% names(out))
})
