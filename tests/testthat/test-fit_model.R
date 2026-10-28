## fit_model() is superseded by run_nlme(); these tests only cover its
## dispatch and output-saving logic, with the backends mocked.
mock_pharmpy_model <- function() {
  structure(
    list(model_code = "$PROBLEM test", dataset = data.frame(ID = 1)),
    class = "pharmpy.model.external.nonmem.model.Model"
  )
}

mock_fit <- function() {
  list(to_dict = function() list(ofv = 123))
}

test_that("fit_model dispatches to the requested tool and saves results", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "fit.rds")
  called <- character(0)
  local_mocked_bindings(
    fit_model_nonmem = function(model, data, ...) {
      called <<- c(called, "nonmem")
      mock_fit()
    },
    fit_model_nlmixr = function(model, data, ...) {
      called <<- c(called, "nlmixr")
      mock_fit()
    }
  )

  fit_model(mock_pharmpy_model(), data = NULL, tool = "nonmem", path = path)
  expect_equal(called, "nonmem")
  expect_equal(readRDS(path), list(ofv = 123))
  md <- readLines(file.path(dir, "fit.md"))
  expect_equal(md[1], "## Model code")
  expect_true("$PROBLEM test" %in% md)

  fit_model(mock_pharmpy_model(), data = NULL, tool = "nlmixr", path = path)
  expect_equal(called, c("nonmem", "nlmixr"))
})

test_that("fit_model rejects non-Pharmpy models and unknown tools", {
  expect_error(
    fit_model(list(), data = NULL, path = tempfile()),
    "Needs a pharmpy model object"
  )
  expect_error(
    fit_model(mock_pharmpy_model(), data = NULL, tool = "foo", path = tempfile()),
    "fit tool foo not supported"
  )
})

test_that("fit_model_nlmixr runs the model through pharmr with the nlmixr tool", {
  captured <- NULL
  local_mocked_bindings(
    run_modelfit = function(model, tool, ...) {
      captured <<- tool
      "fit"
    },
    .package = "pharmr"
  )
  expect_equal(fit_model_nlmixr(mock_pharmpy_model()), "fit")
  expect_equal(captured, "nlmixr")
})

test_that("fit_model with tool = 'nonmem' points to run_nlme()", {
  expect_error(
    fit_model(mock_pharmpy_model(), data = NULL, tool = "nonmem", path = tempfile()),
    "no longer supported"
  )
})
