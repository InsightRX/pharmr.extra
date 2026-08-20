# Common random numbers across uncertainty draws (#131) -----------------------
#
# Every `n_uncertainty` replicate of the `"replicates"` engine must be
# simulated with the *same* seed, so the sequence of standard normal deviates
# behind the simulated ETAs and residuals is shared across draws and the only
# thing that varies between replicates is the parameter vector. That is what
# makes a percentile computed per replicate a clean estimate of parameter
# uncertainty rather than a mixture of that and Monte-Carlo noise.

## Replicated point estimates: stands in for `sample_uncertainty_parameters()`
## when a test wants every replicate to get an *identical* parameter draw, so
## any remaining difference between replicates can only come from the seed.
.identical_draws <- function(model, parameter_estimates, covariance_matrix,
                             n, seed) {
  pe <- unlist(parameter_estimates)
  nms <- intersect(names(pe), colnames(as.matrix(covariance_matrix)))
  as.data.frame(matrix(
    rep(pe[nms], each = n), nrow = n, dimnames = list(NULL, nms)
  ))
}

.crn_nlmixr_case <- function() {
  fx <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")
  dat <- as.data.frame(mod$dataset)
  dat$EVID <- ifelse(dat$AMT > 0, 1, 0)
  dat$MDV  <- ifelse(dat$DV == 0, 1, 0)
  list(
    model = mod,
    data = dat,
    fit = list(
      parameter_estimates = fx$parameter_estimates,
      covariance_matrix   = fx$covariance_matrix
    )
  )
}

test_that("run_sim (nlmixr2): identical draws give identical replicates", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_not_installed("rxode2")
  skip_on_cran()
  ## fixture first: `local_dir()` would break the relative `test_path()`
  case <- .crn_nlmixr_case()
  withr::local_dir(tempdir())
  local_mocked_bindings(
    sample_uncertainty_parameters = .identical_draws,
    .package = "pharmr.extra"
  )

  out <- suppressWarnings(run_sim(
    fit = case$fit, model = case$model, data = case$data, tool = "nlmixr2",
    n_uncertainty = 3, n_iterations = 1, seed = 4321, verbose = FALSE
  ))

  by_rep <- split(out$DV, out$.uncertainty)
  expect_length(by_rep, 3)
  ## Same parameters + same seed => the same simulated subjects, so the
  ## replicates must agree exactly. A per-replicate seed would break this.
  expect_equal(by_rep[[2]], by_rep[[1]])
  expect_equal(by_rep[[3]], by_rep[[1]])
  ## ...and the simulation really is stochastic, so the check above is not
  ## satisfied trivially by a deterministic model: a different base seed gives
  ## different subjects.
  out2 <- suppressWarnings(run_sim(
    fit = case$fit, model = case$model, data = case$data, tool = "nlmixr2",
    n_uncertainty = 3, n_iterations = 1, seed = 9999, verbose = FALSE
  ))
  expect_false(isTRUE(all.equal(split(out2$DV, out2$.uncertainty)[[1]],
                                by_rep[[1]])))
})

test_that("run_sim (nlmixr2): differing draws still differ under a shared seed", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_not_installed("rxode2")
  skip_on_cran()
  ## fixture first: `local_dir()` would break the relative `test_path()`
  case <- .crn_nlmixr_case()
  withr::local_dir(tempdir())
  out <- suppressWarnings(run_sim(
    fit = case$fit, model = case$model, data = case$data, tool = "nlmixr2",
    n_uncertainty = 3, n_iterations = 1, seed = 4321, verbose = FALSE
  ))

  by_rep <- split(out$DV, out$.uncertainty)
  ## Sharing the seed must not collapse the draws: the parameter vectors still
  ## differ, so the replicates do too.
  expect_false(isTRUE(all.equal(by_rep[[2]], by_rep[[1]])))
  expect_false(isTRUE(all.equal(by_rep[[3]], by_rep[[1]])))
})

test_that("run_sim (nlmixr2): every parallel replicate spec carries the base seed", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_not_installed("rxode2")
  skip_on_cran()
  ## fixture first: `local_dir()` would break the relative `test_path()`
  case <- .crn_nlmixr_case()
  withr::local_dir(tempdir())
  seen_specs <- NULL
  local_mocked_bindings(
    ## Run the replicates in-process, but record what the parallel path handed
    ## to the workers.
    parallel_lapply = function(x, fn, n_cores, ...) {
      seen_specs <<- x
      lapply(x, fn)
    },
    .package = "pharmr.extra"
  )

  suppressWarnings(run_sim(
    fit = case$fit, model = case$model, data = case$data, tool = "nlmixr2",
    n_uncertainty = 3, n_iterations = 1, seed = 4321, n_cores = 2,
    verbose = FALSE
  ))

  expect_length(seen_specs, 3)
  expect_equal(vapply(seen_specs, function(s) s$seed, numeric(1)), rep(4321, 3))
  ## the specs do differ where they should: one model per draw
  expect_equal(length(unique(vapply(seen_specs, function(s) s$code, character(1)))), 3)
})

test_that("run_sim (nonmem): every replicate is simulated with the base seed", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  seeds <- c()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    ## The seed reaches NONMEM through the `$SIMULATION` record, which is what
    ## this writes; record it instead of parsing the control stream back.
    set_simulation_clean = function(model, seed, n, ...) {
      seeds <<- c(seeds, seed)
      model
    },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )

  out <- run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
                 n_uncertainty = 3, seed = 777, verbose = FALSE)

  expect_equal(sort(unique(out$.uncertainty)), 1:3)
  ## one $SIMULATION record per replicate (single regimen), all on the same seed
  expect_equal(seeds, rep(777, 3))
})
