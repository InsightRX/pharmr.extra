# Parallel execution of run_sim(n_uncertainty = ) replicates (#126).
#
# The pure-R helper tests run anywhere; the run_sim() tests need Pharmpy (and,
# for the nlmixr2 path, rxode2) and skip otherwise.

test_that("resolve_n_cores validates its input", {
  expect_equal(resolve_n_cores(1), 1L)
  expect_type(resolve_n_cores(1), "integer")
  expect_error(resolve_n_cores(0), "positive integer")
  expect_error(resolve_n_cores(-2), "positive integer")
  expect_error(resolve_n_cores(1.5), "positive integer")
  expect_error(resolve_n_cores("two"), "positive integer")
  expect_error(resolve_n_cores(c(1, 2)), "positive integer")
  ## `as.integer()` returns NA for these; they must still be reported as an
  ## invalid `n_cores`, not blow up on the NA further down.
  expect_error(resolve_n_cores(Inf), "positive integer")
  expect_error(resolve_n_cores(1e10), "positive integer")
})

test_that("worker_threads splits the machine over the workers", {
  avail <- suppressWarnings(parallel::detectCores(logical = TRUE))
  skip_if(is.na(avail))
  expect_equal(worker_threads(1), as.integer(avail))
  expect_equal(worker_threads(2), max(1L, as.integer(avail %/% 2)))
  ## never zero, however many workers are asked for
  expect_gte(worker_threads(avail * 4), 1L)
  expect_type(worker_threads(3), "integer")
})

test_that("resolve_n_cores caps at the number of cores detected", {
  avail <- suppressWarnings(parallel::detectCores(logical = TRUE))
  skip_if(is.na(avail))
  expect_warning(capped <- resolve_n_cores(avail + 10), "exceeds")
  expect_equal(capped, as.integer(avail))
})

test_that("parallel_lapply matches lapply and keeps input order", {
  ## Deliberately uneven work so a load-balanced schedule finishes out of
  ## order: results must still come back indexed by input position.
  fn <- function(i) list(i = i, sq = i^2)
  expected <- lapply(1:6, fn)

  expect_equal(parallel_lapply(1:6, fn, n_cores = 1), expected)

  skip_on_cran()
  cl_ok <- tryCatch({
    cl <- parallel::makePSOCKcluster(2); parallel::stopCluster(cl); TRUE
  }, error = function(e) FALSE)
  skip_if_not(cl_ok, "cannot start a PSOCK cluster")

  expect_equal(parallel_lapply(1:6, fn, n_cores = 2), expected)
})

test_that("run_captured captures warnings and converts errors to values", {
  ok <- run_captured(2, function() {
    warning("first problem")
    warning("second problem")
    data.frame(x = 1)
  })
  expect_equal(ok$index, 2)
  expect_equal(ok$result, data.frame(x = 1))
  ## conditions, not just messages, so custom classes survive the trip back
  expect_length(ok$warnings, 2)
  expect_equal(
    vapply(ok$warnings, conditionMessage, character(1)),
    c("first problem", "second problem")
  )
  expect_s3_class(ok$warnings[[1]], "condition")

  bad <- run_captured(3, function() stop("boom"))
  expect_equal(bad$index, 3)
  expect_s3_class(bad$result, "condition")
  expect_equal(conditionMessage(bad$result), "boom")
})

test_that("emit_replicate_warnings labels warnings and keeps their class", {
  captured <- run_captured(4, function() {
    rlang::warn("something odd", class = "my_custom_warning")
    invisible(NULL)
  })

  expect_warning(
    emit_replicate_warnings(captured$index, captured$warnings),
    "Uncertainty replicate 4: something odd"
  )
  ## a caller handling the original condition class still sees it
  seen <- tryCatch(
    emit_replicate_warnings(captured$index, captured$warnings),
    my_custom_warning = function(w) conditionMessage(w)
  )
  expect_match(seen, "Uncertainty replicate 4")

  expect_silent(emit_replicate_warnings(1, list()))
})

# ---------------------------------------------------------------------------
# Worker path (rxode2 only, no Pharmpy): this is what run_sim() ships to the
# cluster when n_cores > 1.
# ---------------------------------------------------------------------------

## nlmixr2 model code as run_sim() renders it per replicate, parameterised by
## the (drawn) clearance so replicates are distinguishable.
.nlmixr_code <- function(cl) {
  paste0(
    "sim_model <- function() {\n",
    "  ini({\n",
    "    POP_CL <- ", cl, "\n",
    "    POP_V <- 20\n",
    "    eta.cl ~ 0.09\n",
    "    prop_err <- 0.1\n",
    "  })\n",
    "  model({\n",
    "    CL <- POP_CL * exp(eta.cl)\n",
    "    V <- POP_V\n",
    "    d/dt(A1) <- -CL/V * A1\n",
    "    IPRED <- A1 / V\n",
    "    IPRED ~ prop(prop_err)\n",
    "  })\n",
    "}\n",
    "fit <- nlmixr2(sim_model, dataset, est = \"focei\")\n"
  )
}

.rx_sim_dat <- function() {
  data.frame(
    ID   = rep(1:3, each = 4),
    TIME = rep(c(0, 1, 4, 8), 3),
    DV   = 0,
    AMT  = rep(c(100, 0, 0, 0), 3),
    EVID = rep(c(1, 0, 0, 0), 3),
    MDV  = rep(c(1, 0, 0, 0), 3)
  )
}

test_that("run_sim_nlmixr simulates from pre-rendered model code alone", {
  skip_if_not_installed("rxode2")
  out <- run_sim_nlmixr(
    data = .rx_sim_dat(), model_code = .nlmixr_code(1), seed = 11,
    verbose = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_true(all(c("ID", "TIME", "DV", "IPRED") %in% names(out)))
  expect_true(nrow(out) > 0)
  ## no `model`/`fit` needed, but the dataset is then mandatory
  expect_error(
    run_sim_nlmixr(model_code = .nlmixr_code(1), verbose = FALSE),
    "`data` is required"
  )
})

test_that("nlmixr2 replicates give identical results run in parallel or not", {
  skip_if_not_installed("rxode2")
  skip_on_cran()
  cl_ok <- tryCatch({
    cl <- parallel::makePSOCKcluster(2); parallel::stopCluster(cl); TRUE
  }, error = function(e) FALSE)
  skip_if_not(cl_ok, "cannot start a PSOCK cluster")

  ## Mirrors run_sim(): one spec per replicate, each with its own draw and its
  ## own derived seed.
  draws <- c(0.5, 1, 2, 4)
  specs <- lapply(seq_along(draws), function(r) {
    list(index = r, code = .nlmixr_code(draws[r]), seed = 100 + r)
  })
  fn <- make_nlmixr_replicate_fn(
    data = .rx_sim_dat(), n_iterations = 2, variables = NULL,
    add_pk_variables = FALSE, output_file = "simtab"
  )

  seq_res <- parallel_lapply(specs, fn, n_cores = 1)
  par_res <- parallel_lapply(specs, fn, n_cores = 2)

  expect_equal(par_res, seq_res)
  ## order preserved, no replicate lost, none errored
  expect_equal(vapply(par_res, function(x) x$index, numeric(1)), 1:4)
  expect_false(any(vapply(par_res, function(x) inherits(x$result, "condition"),
                          logical(1))))
  ## each replicate really used its own draw
  dv <- lapply(par_res, function(x) x$result$IPRED)
  expect_false(isTRUE(all.equal(dv[[1]], dv[[2]])))
})

test_that("a replicate failing in a worker is captured, not propagated", {
  skip_if_not_installed("rxode2")
  skip_on_cran()
  cl_ok <- tryCatch({
    cl <- parallel::makePSOCKcluster(2); parallel::stopCluster(cl); TRUE
  }, error = function(e) FALSE)
  skip_if_not(cl_ok, "cannot start a PSOCK cluster")

  specs <- list(
    list(index = 1, code = .nlmixr_code(1),  seed = 1),
    list(index = 2, code = "not a model at all", seed = 2),
    list(index = 3, code = .nlmixr_code(2),  seed = 3)
  )
  fn <- make_nlmixr_replicate_fn(
    data = .rx_sim_dat(), n_iterations = 1, variables = NULL,
    add_pk_variables = FALSE, output_file = "simtab"
  )
  res <- parallel_lapply(specs, fn, n_cores = 2)

  expect_length(res, 3)
  expect_s3_class(res[[2]]$result, "condition")
  expect_s3_class(res[[1]]$result, "data.frame")
  expect_s3_class(res[[3]]$result, "data.frame")
})

# ---------------------------------------------------------------------------
# run_sim() integration
# ---------------------------------------------------------------------------

test_that("run_sim (nlmixr2): n_cores > 1 gives identical output to n_cores = 1", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_not_installed("rxode2")
  skip_on_cran()

  fx <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  withr::local_dir(tempdir())

  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")
  fit <- list(
    parameter_estimates = fx$parameter_estimates,
    covariance_matrix   = fx$covariance_matrix
  )
  dat <- as.data.frame(mod$dataset)
  dat$EVID <- ifelse(dat$AMT > 0, 1, 0)
  dat$MDV  <- ifelse(dat$DV == 0, 1, 0)

  args <- list(fit = fit, model = mod, data = dat, tool = "nlmixr2",
               n_uncertainty = 3, n_iterations = 2, seed = 4321,
               verbose = FALSE)
  seq_out <- suppressWarnings(do.call(run_sim, args))
  par_out <- suppressWarnings(do.call(run_sim, c(args, list(n_cores = 2))))

  expect_equal(par_out, seq_out)
  expect_equal(sort(unique(par_out$.uncertainty)), 1:3)
  ## replicates must differ from each other, i.e. each worker really did apply
  ## its own draw rather than the point estimates
  by_rep <- split(par_out$DV, par_out$.uncertainty)
  expect_false(isTRUE(all.equal(by_rep[[1]], by_rep[[2]])))
})

test_that("run_sim: n_cores > 1 falls back to sequential for NONMEM", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
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

  expect_warning(
    out <- run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
                   n_uncertainty = 2, n_cores = 2, verbose = FALSE),
    "nlmixr2"
  )
  expect_equal(sort(unique(out$.uncertainty)), 1:2)
})

test_that("run_sim (nonmem): a failing replicate aborts the run", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  call_n <- 0L
  local_mocked_bindings(
    ## one run_nlme() call per replicate (single regimen): fail the second
    run_nlme = function(...) {
      call_n <<- call_n + 1L
      if(call_n == 2L) stop("simulated NONMEM failure")
      .mock_nlme_result()
    },
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

  ## NONMEM replicate failures are usually systematic (licence, no output
  ## table, clobbered run folder), so the run stops instead of quietly
  ## returning a truncated set of draws.
  expect_error(
    run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
            n_uncertainty = 3, verbose = FALSE),
    "Uncertainty replicate 2 failed"
  )
  ## and it stops there: the third replicate is never started
  expect_equal(call_n, 2L)
})

test_that("run_sim (nlmixr2): a failing replicate is dropped with a warning", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  call_n <- 0L
  local_mocked_bindings(
    run_sim_nlmixr = function(...) {
      call_n <<- call_n + 1L
      if(call_n == 2L) stop("simulated rxode2 failure")
      .mock_sim_table()
    },
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

  out <- suppressWarnings(
    run_sim(fit = fake_fit, model = mod, data = .sim_dat(), tool = "nlmixr2",
            n_uncertainty = 3, verbose = FALSE)
  )
  ## the run survives, and the surviving replicates keep their own indices
  expect_equal(sort(unique(out$.uncertainty)), c(1, 3))
  ## the shortfall is on the result, not only in the console warnings
  expect_equal(attr(out, "n_uncertainty_requested"), 3L)
  expect_equal(attr(out, "n_uncertainty_kept"), 2L)
})

test_that("run_sim: a short replicate set warns about the shortfall", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  call_n <- 0L
  local_mocked_bindings(
    run_sim_nlmixr = function(...) {
      call_n <<- call_n + 1L
      if(call_n == 2L) stop("simulated rxode2 failure")
      .mock_sim_table()
    },
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

  expect_warning(
    ## muffle the per-replicate warning so only the summary one is asserted on
    withCallingHandlers(
      run_sim(fit = fake_fit, model = mod, data = .sim_dat(), tool = "nlmixr2",
              n_uncertainty = 3, verbose = FALSE),
      warning = function(w) {
        if(grepl("omitted", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    "Only 2 of 3 uncertainty replicates"
  )
})

test_that("run_sim: errors when every replicate fails", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  local_mocked_bindings(
    run_sim_nlmixr = function(...) stop("simulated rxode2 failure"),
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

  expect_error(
    suppressWarnings(
      run_sim(fit = fake_fit, model = mod, data = .sim_dat(), tool = "nlmixr2",
              n_uncertainty = 2, verbose = FALSE)
    ),
    "All 2 uncertainty replicates failed"
  )
})

test_that("run_sim: `data` is validated before replicates are dispatched", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  ## One clear message from the parent, rather than n_uncertainty worker
  ## failures followed by "all replicates failed".
  expect_error(
    run_sim(fit = fake_fit, model = mod, data = matrix(1:4, ncol = 2),
            tool = "nlmixr2", n_uncertainty = 2, n_cores = 2, verbose = FALSE),
    "must be a data.frame"
  )
})
