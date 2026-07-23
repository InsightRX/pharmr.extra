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

  run_nlme_fn <- mockery::mock(NULL)
  stub(call_pharmpy_tool, "run_nlme", run_nlme_fn)
  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "create_run_folder", function(...) withr::local_tempdir())
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  stub(call_pharmpy_tool, "withr::with_dir", function(...) stop("abort before pharmpy"))

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
