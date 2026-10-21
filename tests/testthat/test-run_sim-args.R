# run_sim() argument handling and its small helpers (#155).
#
# None of these tests need Pharmpy, NONMEM or rxode2: the model is a plain R
# list carrying the Pharmpy model class, and every step that would reach into
# Python or a simulation engine is mocked out. They cover the branches of
# run_sim() that decide *what* to run, so they run everywhere, CI included.

## A stand-in for a Pharmpy model object: run_sim() only checks the class and
## reads `$dataset` / `$code` off it on the paths exercised here.
.fake_pharmpy_model <- function(dataset = .sim_dat(),
                                class = "pharmpy.model.model.Model") {
  structure(list(code = "$PROBLEM fake", dataset = dataset), class = class)
}

## Mock the nlmixr2 engine, recording the arguments of every call.
local_mock_run_sim_nlmixr <- function(fn = NULL, .local_envir = parent.frame()) {
  calls <- new.env()
  calls$args <- list()
  if (is.null(fn)) fn <- function(...) .mock_sim_table()
  testthat::local_mocked_bindings(
    run_sim_nlmixr = function(...) {
      calls$args[[length(calls$args) + 1]] <- list(...)
      fn(...)
    },
    .package = "pharmr.extra",
    .env = .local_envir
  )
  calls
}

# ── validate_sim_data() ─────────────────────────────────────────────────────

test_that("validate_sim_data accepts NULL and data.frames", {
  expect_null(validate_sim_data(NULL))
  expect_null(validate_sim_data(.sim_dat()))
  expect_null(validate_sim_data(dplyr::as_tibble(.sim_dat())))
})

test_that("validate_sim_data rejects anything that is not a data.frame", {
  expect_error(validate_sim_data("data.csv"), "must be a data.frame")
  expect_error(validate_sim_data(list(ID = 1, TIME = 0)), "must be a data.frame")
  expect_error(validate_sim_data(as.matrix(.sim_dat())), "must be a data.frame")
  ## The class it got is named, so the caller can see what went wrong.
  expect_error(validate_sim_data(1:3), "integer")
})

# ── create_obs_records() ────────────────────────────────────────────────────

.obs_template_data <- function() {
  data.frame(
    ID       = c(1, 1, 1, 2, 2),
    TIME     = c(0, 0, 4, 0, 4),
    AMT      = c(100, 0, 0, 100, 0),
    EVID     = c(1, 0, 0, 1, 0),
    MDV      = c(1, 1, 0, 1, 0),
    CMT      = c(1, 3, 2, 1, 2),
    .regimen = "A"
  )
}

test_that("create_obs_records builds one sampling design per subject", {
  out <- create_obs_records(.obs_template_data(), t_obs = c(1, 2, 8),
                            n_subjects = 2, model = NULL)
  expect_equal(nrow(out), 6)
  expect_equal(out$ID, rep(c(1, 2), each = 3))
  expect_equal(out$TIME, rep(c(1, 2, 8), 2))
  expect_true(all(out$EVID == 0 & out$MDV == 0 & out$AMT == 0))
  expect_true(all(out$DV == 0 & out$RATE == 0))
  expect_equal(unique(out$.regimen), "A")
})

test_that("create_obs_records takes CMT from subject 1's first observation", {
  ## With MDV, the first *non-missing* observation decides (CMT 2, not the
  ## MDV = 1 record in CMT 3).
  out <- create_obs_records(.obs_template_data(), t_obs = 1,
                            n_subjects = 1, model = NULL)
  expect_equal(out$CMT, 2)

  ## Without MDV, the first observation record does.
  dat <- .obs_template_data()
  dat$MDV <- NULL
  out <- create_obs_records(dat, t_obs = 1, n_subjects = 1, model = NULL)
  expect_equal(out$CMT, 3)
})

test_that("create_obs_records falls back to the model's observation compartment", {
  seen <- NULL
  local_mocked_bindings(
    get_obs_compartment = function(model) {
      seen <<- model
      5
    }
  )
  dat <- .obs_template_data()
  dat$CMT[dat$ID == 1 & dat$EVID == 0] <- NA
  dat$MDV <- NULL
  out <- create_obs_records(dat, t_obs = c(1, 2), n_subjects = 1,
                            model = "the model")
  expect_equal(out$CMT, c(5, 5))
  expect_equal(seen, "the model")

  ## Same when subject 1 has no observation record at all.
  dat <- .obs_template_data()
  dat <- dat[dat$EVID == 1, ]
  out <- create_obs_records(dat, t_obs = 1, n_subjects = 1, model = "the model")
  expect_equal(out$CMT, 5)
})

test_that("create_obs_records adds new IDs when asked for more subjects", {
  out <- create_obs_records(.obs_template_data(), t_obs = 1,
                            n_subjects = 4, model = NULL)
  expect_equal(out$ID, c(1, 2, 3, 4))
})

test_that("create_obs_records repeats the design for every regimen", {
  dat <- .obs_template_data()
  dat <- rbind(dat, transform(dat, .regimen = "B"))
  out <- create_obs_records(dat, t_obs = c(1, 2), n_subjects = 2, model = NULL)
  expect_equal(nrow(out), 8)
  expect_equal(as.vector(table(out$.regimen)), c(4, 4))
  expect_equal(out$ID[out$.regimen == "B"], c(1, 1, 2, 2))
})

# ── make_nlmixr_replicate_fn() ──────────────────────────────────────────────

test_that("make_nlmixr_replicate_fn forwards its settings and the spec", {
  calls <- local_mock_run_sim_nlmixr()
  dat <- .sim_dat()
  fn <- make_nlmixr_replicate_fn(
    data = dat, n_iterations = 2, variables = c("CL", "V"),
    add_pk_variables = TRUE, output_file = "mytab"
  )
  res <- fn(list(index = 7L, code = "model code", seed = 99))

  expect_equal(res$index, 7L)
  expect_equal(res$result, .mock_sim_table())
  expect_length(calls$args, 1)
  args <- calls$args[[1]]
  expect_equal(args$data, dat)
  expect_equal(args$model_code, "model code")
  expect_equal(args$seed, 99)
  expect_equal(args$n_iterations, 2)
  expect_equal(args$variables, c("CL", "V"))
  expect_true(args$add_pk_variables)
  expect_equal(args$output_file, "mytab")
  expect_false(args$verbose)
})

test_that("make_nlmixr_replicate_fn returns a failing replicate as a value", {
  local_mock_run_sim_nlmixr(function(...) {
    warning("draw looks odd")
    stop("rxSolve failed")
  })
  fn <- make_nlmixr_replicate_fn(
    data = .sim_dat(), n_iterations = 1, variables = NULL,
    add_pk_variables = FALSE, output_file = "simtab"
  )
  res <- fn(list(index = 3L, code = "x", seed = 1))
  expect_equal(res$index, 3L)
  expect_s3_class(res$result, "error")
  expect_match(conditionMessage(res$result), "rxSolve failed")
  expect_length(res$warnings, 1)
})

test_that("make_nlmixr_replicate_fn does not keep the caller's frame alive", {
  ## The returned closure is shipped to worker processes, so it must close over
  ## the factory's own arguments only, not whatever the caller had in scope.
  fn <- local({
    pharmpy_object <- "must not travel"
    make_nlmixr_replicate_fn(
      data = .sim_dat(), n_iterations = 1, variables = NULL,
      add_pk_variables = FALSE, output_file = "simtab"
    )
  })
  env <- environment(fn)
  expect_setequal(
    ls(env),
    c("data", "n_iterations", "variables", "add_pk_variables", "output_file")
  )
  expect_false(exists("pharmpy_object", envir = env, inherits = FALSE))
})

# ── run_sim(): model / fit resolution ──────────────────────────────────────

test_that("run_sim errors when the fit carries no model", {
  expect_error(
    run_sim(fit = list(), verbose = FALSE),
    "No proper model object available"
  )
})

test_that("run_sim takes the model from the fit's final_model attribute", {
  calls <- local_mock_run_sim_nlmixr()
  mod <- .fake_pharmpy_model()
  fit <- structure(list(), final_model = mod)
  out <- run_sim(fit = fit, tool = "nlmixr2", verbose = FALSE)
  expect_equal(out, .mock_sim_table())
  expect_identical(calls$args[[1]]$model, mod)
  expect_identical(calls$args[[1]]$fit, fit)
})

test_that("run_sim loads a model file through create_model_from_file", {
  calls <- local_mock_run_sim_nlmixr()
  seen <- list()
  local_mocked_bindings(
    create_model_from_file = function(model_file, data = NULL, ...) {
      seen[[length(seen) + 1]] <<- list(
        model_file = model_file,
        data = if (!is.null(data)) read.csv(data)
      )
      .fake_pharmpy_model()
    }
  )

  ## Without data: only the model file is handed over.
  expect_message(
    run_sim(model = "run1.mod", tool = "nlmixr2", verbose = FALSE),
    "successfully imported"
  )
  expect_equal(seen[[1]]$model_file, "run1.mod")
  expect_null(seen[[1]]$data)

  ## With a data.frame: it is written to a csv the model is loaded with.
  dat <- .sim_dat()
  suppressMessages(
    run_sim(model = "run1.mod", data = dat, tool = "nlmixr2", verbose = FALSE)
  )
  expect_equal(seen[[2]]$data, dat)
  expect_equal(calls$args[[2]]$data, dat)
})

test_that("run_sim aborts when a model file cannot be loaded into Pharmpy", {
  local_mocked_bindings(create_model_from_file = function(...) NULL)
  expect_error(
    suppressMessages(run_sim(model = "broken.mod", verbose = FALSE)),
    "Could not load model into Pharmpy"
  )
})

# ── run_sim(): tool resolution ─────────────────────────────────────────────

test_that("run_sim(tool = 'auto') sends nlmixr models to the nlmixr2 engine", {
  calls <- local_mock_run_sim_nlmixr()
  mod <- .fake_pharmpy_model(class = c(
    "pharmpy.model.external.nlmixr.model.Model", "pharmpy.model.model.Model"
  ))
  out <- suppressMessages(run_sim(model = mod, verbose = FALSE))
  expect_equal(out, .mock_sim_table())
  expect_length(calls$args, 1)
})

test_that("run_sim aborts on a tool it cannot simulate with", {
  local_mocked_bindings(get_tool_from_model = function(model) "monolix")
  expect_error(
    suppressMessages(run_sim(model = .fake_pharmpy_model(), verbose = FALSE)),
    "Unsupported simulation tool: monolix"
  )
  expect_error(
    suppressMessages(run_sim(model = .fake_pharmpy_model(), tool = "psn")),
    "should be one of"
  )
})

test_that("run_sim forwards its arguments to the nlmixr2 engine", {
  calls <- local_mock_run_sim_nlmixr()
  dat <- .sim_dat()
  suppressMessages(run_sim(
    model = .fake_pharmpy_model(), data = dat, tool = "nlmixr2",
    id = "sim_x", path = "some/path", n_iterations = 3,
    variables = c("CL"), add_pk_variables = TRUE, output_file = "out",
    seed = 42, verbose = FALSE
  ))
  args <- calls$args[[1]]
  expect_equal(args$data, dat)
  expect_equal(args$id, "sim_x")
  expect_equal(args$path, "some/path")
  expect_equal(args$n_iterations, 3)
  expect_equal(args$variables, "CL")
  expect_true(args$add_pk_variables)
  expect_equal(args$output_file, "out")
  expect_equal(args$seed, 42)
  expect_false(args$verbose)
})

test_that("run_sim checks `data` before running anything", {
  calls <- local_mock_run_sim_nlmixr()
  expect_error(
    suppressMessages(run_sim(model = .fake_pharmpy_model(), data = "data.csv",
                             tool = "nlmixr2")),
    "must be a data.frame"
  )
  expect_length(calls$args, 0)
})

# ── run_sim(): n_uncertainty validation ────────────────────────────────────

test_that("run_sim rejects an n_uncertainty that is not a non-negative integer", {
  calls <- local_mock_run_sim_nlmixr()
  fit <- list(parameter_estimates = c(A = 1), covariance_matrix = diag(1))
  for (bad in list(-1, 0.5, "abc", c(1, 2), NA, .Machine$integer.max + 1)) {
    expect_error(
      suppressMessages(run_sim(fit = fit, model = .fake_pharmpy_model(),
                               tool = "nlmixr2", n_uncertainty = bad)),
      "must be a non-negative integer",
      info = paste(format(bad), collapse = ",")
    )
  }
  expect_length(calls$args, 0)
})

test_that("run_sim(n_uncertainty = 0) runs the point estimate", {
  calls <- local_mock_run_sim_nlmixr()
  local_mocked_bindings(
    sample_uncertainty_parameters = function(...) stop("must not sample")
  )
  out <- suppressMessages(run_sim(
    model = .fake_pharmpy_model(), tool = "nlmixr2", n_uncertainty = 0,
    verbose = FALSE
  ))
  expect_false(".uncertainty" %in% names(out))
  expect_length(calls$args, 1)
  ## A numeric string counts as the number it holds.
  out <- suppressMessages(run_sim(
    model = .fake_pharmpy_model(), tool = "nlmixr2", n_uncertainty = "0",
    verbose = FALSE
  ))
  expect_false(".uncertainty" %in% names(out))
})

test_that("run_sim(n_uncertainty = ) needs a fit with a covariance matrix", {
  mod <- .fake_pharmpy_model()
  ## No fit at all.
  expect_error(
    suppressMessages(run_sim(model = mod, tool = "nlmixr2", n_uncertainty = 2)),
    "covariance matrix"
  )
  ## A fit without a covariance matrix ...
  expect_error(
    suppressMessages(run_sim(
      fit = list(parameter_estimates = c(A = 1)), model = mod,
      tool = "nlmixr2", n_uncertainty = 2
    )),
    "covariance matrix"
  )
  ## ... or without parameter estimates.
  expect_error(
    suppressMessages(run_sim(
      fit = list(covariance_matrix = diag(1)), model = mod,
      tool = "nlmixr2", n_uncertainty = 2
    )),
    "covariance matrix"
  )
})

# ── run_sim(): sequential nlmixr2 uncertainty replicates ───────────────────

test_that("run_sim (nlmixr2): replicates are tagged, and failures dropped and counted", {
  ## Replicate 2 fails, replicate 3 returns an empty table; 1 and 4 survive.
  draws <- data.frame(POP_CL = c(1, 2, 3, 4))
  local_mocked_bindings(
    sample_uncertainty_parameters = function(model, parameter_estimates,
                                             covariance_matrix, n, seed) {
      draws[seq_len(n), , drop = FALSE]
    },
    rerender_nlmixr_code = function(model, new_model) {
      paste("code for", new_model$draw)
    }
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) {
      model$draw <- inits$POP_CL
      model
    },
    .package = "pharmr"
  )
  calls <- local_mock_run_sim_nlmixr(function(..., model) {
    switch(
      as.character(model$draw),
      "2" = stop("solver blew up"),
      "3" = data.frame(),
      .mock_sim_table()
    )
  })
  fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = matrix(1, dimnames = list("POP_CL", "POP_CL"))
  )

  warns <- character()
  out <- withCallingHandlers(
    run_sim(fit = fit, model = .fake_pharmpy_model(), tool = "nlmixr2",
            n_uncertainty = 4, seed = 11, verbose = FALSE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(sort(unique(out$.uncertainty)), c(1L, 4L))
  expect_equal(attr(out, "n_uncertainty_requested"), 4L)
  expect_equal(attr(out, "n_uncertainty_kept"), 2L)
  ## Each replicate simulates its own draw, all with the same seed, and the
  ## cached nlmixr2 code is regenerated for the draw.
  expect_length(calls$args, 4)
  expect_equal(vapply(calls$args, function(a) a$model$draw, numeric(1)), 1:4)
  expect_equal(vapply(calls$args, function(a) a$seed, numeric(1)), rep(11, 4))
  expect_equal(attr(calls$args[[4]]$model, "nlmixr_code"), "code for 4")

  expect_true(any(grepl("POP_V", warns)))                  # held fixed
  expect_true(any(grepl("replicate 2 failed", warns)))
  expect_true(any(grepl("replicate 3 produced no simulation output", warns)))
  expect_true(any(grepl("Only 2 of 4", warns)))
})

test_that("run_sim (nlmixr2): aborts when no replicate produced output", {
  local_mocked_bindings(
    sample_uncertainty_parameters = function(model, parameter_estimates,
                                             covariance_matrix, n, seed) {
      data.frame(POP_CL = seq_len(n))
    },
    rerender_nlmixr_code = function(model, new_model) "code"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )
  local_mock_run_sim_nlmixr(function(...) data.frame())
  fit <- list(
    parameter_estimates = c(POP_CL = 1),
    covariance_matrix   = matrix(1, dimnames = list("POP_CL", "POP_CL"))
  )
  expect_error(
    suppressWarnings(run_sim(
      fit = fit, model = .fake_pharmpy_model(), tool = "nlmixr2",
      n_uncertainty = 2, verbose = FALSE
    )),
    "All 2 uncertainty replicates failed"
  )
})

# ── run_sim(): NONMEM point-estimate path ──────────────────────────────────

test_that("run_sim (nonmem): tags subproblems and adds PK variables per regimen", {
  withr::local_dir(withr::local_tempdir())
  tagged <- list()
  pk_args <- list()
  local_mocked_bindings(
    build_nonmem_sim_model = function(model, ...) model,
    get_dv_scale_factor = function(model, ...) 2.5,
    get_default_dose_compartment = function(code) 1,
    run_nlme = function(...) .mock_nlme_result(),
    tag_sim_subproblems = function(tab, file, n_iterations) {
      tagged[[length(tagged) + 1]] <<- list(file = file, n = n_iterations)
      tab
    },
    calc_pk_variables = function(data, regimen, dv_scale) {
      pk_args[[length(pk_args) + 1]] <<- list(regimen = regimen,
                                              dv_scale = dv_scale)
      data
    }
  )
  dat <- rbind(
    transform(.sim_dat(), .regimen = "low"),
    transform(.sim_dat(), .regimen = "high")
  )
  out <- run_sim(
    model = .fake_pharmpy_model(), data = dat, tool = "nonmem", id = "sim_t",
    n_iterations = 2, add_pk_variables = TRUE, verbose = FALSE
  )

  expect_equal(unique(out$regimen_label), c("low", "high"))
  expect_equal(
    vapply(tagged, `[[`, character(1), "file"),
    file.path(getwd(), "sim_t", paste0("regimen_", 1:2), "simtab")
  )
  expect_equal(vapply(tagged, `[[`, numeric(1), "n"), c(2, 2))
  ## AUC_SS is scaled by the model's dv scale, with each regimen's own doses.
  expect_equal(vapply(pk_args, `[[`, numeric(1), "dv_scale"), c(2.5, 2.5))
  expect_equal(pk_args[[1]]$regimen$dose, 100)
})

test_that("run_sim (nonmem): warns when a regimen's results hold no table", {
  withr::local_dir(withr::local_tempdir())
  local_mocked_bindings(
    build_nonmem_sim_model = function(model, ...) model,
    ## The table is there for run_sim()'s failure check, but not under the
    ## name the results are later read back by.
    run_nlme = function(...) {
      res <- .mock_nlme_result()
      attr(res, "tables") <- list(NULL)
      attr(res, "tables")[[1]] <- .mock_sim_tab()
      names(attr(res, "tables")) <- NA
      res
    }
  )
  expect_warning(
    out <- run_sim(model = .fake_pharmpy_model(), data = .sim_dat(),
                   tool = "nonmem", verbose = FALSE),
    "did not output any results"
  )
  expect_equal(nrow(out), 0)
})
