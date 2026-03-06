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
