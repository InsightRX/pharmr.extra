test_that("call_pharmpy_fit reads, fits and copies results back as run.*", {
  path <- withr::local_tempdir()
  writeLines("$PROBLEM test", file.path(path, "run.mod"))
  run_folder <- file.path(path, "modelfit1", "models", "run")

  calls <- list()
  local_mocked_bindings(
    clean_pharmpy_runfolders = function(id, folder, tool, remove, ...) {
      calls$clean <<- list(folder = folder, tool = tool, remove = remove)
      invisible(NULL)
    },
    get_pharmpy_runfolders = function(id, folder, tool) "modelfit1"
  )
  local_mocked_bindings(
    read_model = function(path, ...) {
      calls$read <<- c(calls$read, path)
      "model_object"
    },
    fit = function(model, ...) {
      calls$fit <<- list(model = model, wd = getwd())
      ## mimic Pharmpy writing results into the modelfit run folder
      dir.create(run_folder, recursive = TRUE)
      writeLines("lst", file.path(run_folder, "model.lst"))
      writeLines("ext", file.path(run_folder, "model.ext"))
      invisible(NULL)
    },
    .package = "pharmr"
  )

  expect_message(
    call_pharmpy_fit("run.mod", path, clean = FALSE),
    "Starting Pharmpy modelfit"
  )
  expect_equal(calls$clean, list(folder = path, tool = "modelfit", remove = FALSE))
  expect_equal(calls$read, file.path(path, "run.mod"))
  expect_equal(calls$fit$model, "model_object")
  expect_equal(normalizePath(calls$fit$wd), normalizePath(path))
  expect_equal(readLines(file.path(path, "run.lst")), "lst")
  expect_equal(readLines(file.path(path, "run.ext")), "ext")
})

test_that("call_pharmpy_fit reads every model when given several files", {
  path <- withr::local_tempdir()
  fitted <- NULL
  local_mocked_bindings(
    clean_pharmpy_runfolders = function(...) invisible(NULL),
    get_pharmpy_runfolders = function(...) character(0)
  )
  local_mocked_bindings(
    read_model = function(path, ...) basename(path),
    fit = function(model, ...) {
      fitted <<- model
      invisible(NULL)
    },
    .package = "pharmr"
  )
  call_pharmpy_fit(c("run1.mod", "run2.mod"), path, verbose = FALSE)
  expect_equal(fitted, list("run1.mod", "run2.mod"))
})
