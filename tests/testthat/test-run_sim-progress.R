## Regression tests for issue #137: the uncertainty-replicate progress bar in
## run_sim() turned a *finished* set of replicates into an error under Rscript.
##
## Every other run_sim() test passes `verbose = FALSE`, which skips the bar
## entirely -- which is why the suite never saw this. These force the bar on.

test_that("run_sim (stub): a stray cli_process_done() in the backend cannot fail a finished run", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())
  local_cli_progress_forced()

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  ## The shape of the original bug: `call_nmfe()` called `cli_process_done()`
  ## even when `verbose = FALSE` had opened no status bar, so each replicate
  ## popped run_sim()'s own progress bar off cli's stack. The sleep is what
  ## gets the bar past `cli.progress_show_after` and actually rendered.
  local_mock_nonmem_sim(function(spec, nmfe, table_names, clean = TRUE) {
    Sys.sleep(0.05)
    cli::cli_process_done()
    .mock_sim_tab()
  })
  local_mocked_bindings(
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )

  out <- run_sim(
    fit = fake_fit, model = mod, data = .sim_dat(),
    n_uncertainty = 3, uncertainty_engine = "replicates", verbose = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(sort(unique(out$.uncertainty)), 1:3)
})

test_that("run_sim (stub): sequential uncertainty replicates run with verbose = TRUE", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())
  local_cli_progress_forced()

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  local_mock_nonmem_sim(function(spec, nmfe, table_names, clean = TRUE) {
    Sys.sleep(0.05)
    .mock_sim_tab()
  })
  local_mocked_bindings(
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )

  out <- run_sim(
    fit = fake_fit, model = mod, data = .sim_dat(),
    n_uncertainty = 2, uncertainty_engine = "replicates", verbose = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(sort(unique(out$.uncertainty)), 1:2)
})

test_that("progress_try() swallows a failing progress-bar call", {
  ## The bar is cosmetic; nothing it does may propagate out of run_sim().
  expect_null(progress_try(stop("cli exploded")))
  expect_equal(progress_try(41 + 1), 42)
})
