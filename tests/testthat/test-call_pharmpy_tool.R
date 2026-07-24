library(mockery)

# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

test_that("call_pharmpy_tool calls remove_tables_from_model when remove_tables = TRUE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)
  expect_true(length(get_tables_in_model_code(mod$code)) > 0)

  remove_fn <- mockery::mock(mod)
  stub(call_pharmpy_tool, "remove_tables_from_model", remove_fn)
  stub(call_pharmpy_tool, "create_run_folder", function(...) withr::local_tempdir())
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  stub(call_pharmpy_tool, "withr::with_dir", function(...) stop("abort before pharmpy"))

  tryCatch(
    call_pharmpy_tool(
      id = "test_remove_run",
      model = mod,
      tool = "bootstrap",
      remove_tables = TRUE,
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  mockery::expect_called(remove_fn, 1)
})

test_that("call_pharmpy_tool auto-generates results for structsearch (req_results member)", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)

  ## run_nlme returns a sentinel so we can assert the auto-generated results
  ## actually propagate into the pharmpy tool call (not just that run_nlme
  ## was invoked). A plain string carries no "model" attr, so the
  ## `attr(results, "model")` branch is skipped and `model` is left intact.
  fake_results <- "GENERATED_RESULTS_SENTINEL"
  run_nlme_fn <- mockery::mock(fake_results)

  ## Persistent run folder (a stub-local tempdir would be cleaned up on stub
  ## return, breaking the real withr::with_dir(run_folder) downstream).
  run_dir <- withr::local_tempdir()

  captured <- NULL
  stub(call_pharmpy_tool, "run_nlme", run_nlme_fn)
  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "create_run_folder", function(...) run_dir)
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  ## Capture the arguments handed to the pharmpy tool at the do.call boundary.
  stub(call_pharmpy_tool, "do.call", function(what, args, ...) {
    captured <<- list(what = what, args = args)
    stop("captured before pharmpy call")
  })

  tryCatch(
    call_pharmpy_tool(
      id = "test_structsearch_run",
      model = mod,
      tool = "structsearch",
      options = list(type = "tmdd"),
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  # structsearch is in req_results, so no `results` provided -> run_nlme called
  mockery::expect_called(run_nlme_fn, 1)
  # ...and the generated results are passed through to run_structsearch
  expect_equal(captured$what, "run_structsearch")
  expect_equal(captured$args$results, fake_results)
})

test_that("call_pharmpy_tool does not call remove_tables_from_model when remove_tables = FALSE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)

  remove_fn <- mockery::mock(mod)
  stub(call_pharmpy_tool, "remove_tables_from_model", remove_fn)
  stub(call_pharmpy_tool, "create_run_folder", function(...) withr::local_tempdir())
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  stub(call_pharmpy_tool, "withr::with_dir", function(...) stop("abort before pharmpy"))

  tryCatch(
    call_pharmpy_tool(
      id = "test_preserve_run",
      model = mod,
      tool = "bootstrap",
      remove_tables = FALSE,
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  mockery::expect_called(remove_fn, 0)
})

test_that("call_pharmpy_tool passes type/dv_types/results through to run_structsearch", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)

  ## Provide a fake `results` so run_nlme (NONMEM) is not invoked; the model
  ## is taken from the results object (attr "model").
  fake_results <- mod
  attr(fake_results, "model") <- mod

  ## Persistent run folder (local_tempdir would be cleaned up on stub return,
  ## breaking the real withr::with_dir(run_folder) setwd downstream).
  run_dir <- withr::local_tempdir()

  captured <- NULL
  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "create_run_folder", function(...) run_dir)
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  ## Capture the arguments handed to the pharmpy tool at the do.call boundary.
  stub(call_pharmpy_tool, "do.call", function(what, args, ...) {
    captured <<- list(what = what, args = args)
    stop("captured before pharmpy call")
  })

  tryCatch(
    call_pharmpy_tool(
      id = "test_structsearch_args",
      model = mod,
      results = fake_results,
      tool = "structsearch",
      options = list(type = "tmdd", dv_types = list(drug = 1, target = 2, complex = 3)),
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  expect_equal(captured$what, "run_structsearch")
  expect_equal(captured$args$type, "tmdd")
  expect_equal(captured$args$dv_types, list(drug = 1, target = 2, complex = 3))
  expect_false(is.null(captured$args$results))
})

## ---- seed_tmdd_results() ----------------------------------------------------
## These exercise the pure-R seeding logic without NONMEM/Pharmpy by stubbing
## reticulate::import so pd$Series() is the identity and dc$replace() captures
## the augmented parameter_estimates it is handed.

# Fake pandas/dataclasses so the Series/replace calls don't touch Python.
# dc$replace() stashes the parameter_estimates into `captured_pe` in the caller.
fake_import_capturing <- function(env) {
  function(module, ...) {
    if (identical(module, "pandas")) {
      list(Series = function(x) x)  # identity: as.list(new_vals) passes through
    } else {                        # dataclasses
      list(replace = function(object, parameter_estimates) {
        env$captured_pe <- parameter_estimates
        object
      })
    }
  }
}

test_that("seed_tmdd_results returns results unchanged for an MM-parameterised base", {
  results <- list(parameter_estimates = c(POP_CL = 2, POP_KM = 9, POP_CLMM = 3))
  res <- seed_tmdd_results(results, kd = 7, verbose = FALSE)
  expect_identical(res, results)
})

test_that("seed_tmdd_results warns and returns unchanged when POP_CL is absent", {
  results <- list(parameter_estimates = c(POP_VC = 5))
  expect_message(
    res <- seed_tmdd_results(results, kd = 7, verbose = FALSE),
    "POP_CL"
  )
  expect_identical(res, results)
})

test_that("seed_tmdd_results seeds POP_KM (= kd) and POP_CLMM (= POP_CL) when both missing", {
  env <- new.env()
  results <- list(parameter_estimates = c(POP_CL = 2, POP_VC = 5))
  stub(seed_tmdd_results, "reticulate::import", fake_import_capturing(env))

  seed_tmdd_results(results, kd = 7, verbose = FALSE)

  expect_equal(env$captured_pe$POP_KM, 7)     # seeded from kd
  expect_equal(env$captured_pe$POP_CLMM, 2)   # seeded from the fitted POP_CL
  expect_equal(env$captured_pe$POP_CL, 2)     # original estimates preserved
  expect_equal(env$captured_pe$POP_VC, 5)
})

test_that("seed_tmdd_results keeps a fitted POP_KM for a partial-MM base (no clobbering)", {
  env <- new.env()
  results <- list(parameter_estimates = c(POP_CL = 2, POP_KM = 9))
  stub(seed_tmdd_results, "reticulate::import", fake_import_capturing(env))

  seed_tmdd_results(results, kd = 7, verbose = FALSE)

  expect_equal(env$captured_pe$POP_KM, 9)                        # fitted value kept, not overwritten by kd
  expect_equal(env$captured_pe$POP_CLMM, 2)                      # only the missing one is added
  expect_equal(sum(names(env$captured_pe) == "POP_KM"), 1L)      # no duplicate index
})

test_that("call_pharmpy_tool strips `kd` before forwarding args to run_structsearch", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)

  fake_results <- mod
  attr(fake_results, "model") <- mod
  run_dir <- withr::local_tempdir()

  captured <- NULL
  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "create_run_folder", function(...) run_dir)
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  ## Avoid the Python seeding path; we only care that `kd` is dropped from args.
  stub(call_pharmpy_tool, "seed_tmdd_results", function(results, ...) results)
  stub(call_pharmpy_tool, "do.call", function(what, args, ...) {
    captured <<- list(what = what, args = args)
    stop("captured before pharmpy call")
  })

  tryCatch(
    call_pharmpy_tool(
      id = "test_structsearch_kd",
      model = mod,
      results = fake_results,
      tool = "structsearch",
      options = list(type = "tmdd", kd = 5),
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  expect_equal(captured$what, "run_structsearch")
  expect_true(is.null(captured$args$kd))         # kd is a pharmr.extra convenience, not forwarded
  expect_false("kd" %in% names(captured$args))
  expect_false(is.null(captured$args$results))   # results still propagate
})
