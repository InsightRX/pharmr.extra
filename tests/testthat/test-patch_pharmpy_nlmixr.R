# Tests for the Python monkey-patches that make Pharmpy's nlmixr backend usable
# from `call_pharmpy_tool()` (#121). They exercise the patched module
# attributes directly; the end-to-end tool runs need nlmixr2 plus a fitted
# model and live in the pharmaair integration suite.

skip_if_pharmpy_nlmixr_not_available <- function() {
  testthat::skip_if_not(
    reticulate::py_module_available("pharmpy.tools.external.nlmixr.run"),
    "Pharmpy nlmixr backend not available"
  )
}

test_that("patch is idempotent and marks the Pharmpy module", {
  skip_if_pharmpy_nlmixr_not_available()

  expect_silent(patch_pharmpy_nlmixr_results())
  expect_silent(patch_pharmpy_nlmixr_results())

  run <- reticulate::import("pharmpy.tools.external.nlmixr.run", convert = TRUE)
  expect_true(isTRUE(run$`_pharmr_extra_nlmixr_patched`))
})

test_that("get_thetas().names is a list, so pandas .loc does not see a tuple", {
  skip_if_pharmpy_nlmixr_not_available()
  patch_pharmpy_nlmixr_results()

  ## Upstream indexes the thetas table with `get_thetas(model).names`. That is
  ## a tuple in Pharmpy >= 2.0, and pandas reads a tuple passed to `.loc` as a
  ## multi-axis indexer -> `IndexingError: Too many indexers`.
  py <- reticulate::py_run_string("
import pandas as _pd
from pharmpy.modeling import load_example_model as _load
import pharmpy.tools.external.nlmixr.run as _run

_model = _load('pheno')
_thetas = _run.get_thetas(_model)
_names = _thetas.names
_is_list = isinstance(_names, list)
_n = len(_thetas)
_df = _pd.DataFrame({'fit$theta': range(len(_names))}, index=_names)
_nrow = len(_df.loc[_names])
", convert = TRUE)

  expect_true(py$`_is_list`)
  expect_equal(py$`_nrow`, length(py$`_names`))
  ## delegation to the real Parameters object is intact
  expect_equal(py$`_n`, length(py$`_names`))
})

test_that("dataset writer writes <model name>.csv into a directory", {
  skip_if_pharmpy_nlmixr_not_available()
  patch_pharmpy_nlmixr_results()

  run <- reticulate::import("pharmpy.tools.external.nlmixr.run", convert = FALSE)
  modeling <- reticulate::import("pharmpy.modeling", convert = FALSE)
  model <- modeling$load_example_model("pheno")

  ## Pharmpy >= 2.0 renamed write_csv() to write_dataset()
  writer <- if(reticulate::py_has_attr(run, "write_dataset")) {
    run$write_dataset
  } else {
    run$write_csv
  }

  tmp_dir <- withr::local_tempdir()
  writer(model, path = tmp_dir)

  ## `execute_model()` generates `read.csv("<path>/<model name>.csv")`, so that
  ## is the name the dataset has to be written under — not the datainfo name
  ## ("pheno.dta" for the example model).
  expect_equal(dir(tmp_dir), "pheno.csv")
})

test_that("dataset writer keeps the caller's `force`", {
  skip_if_pharmpy_nlmixr_not_available()
  patch_pharmpy_nlmixr_results()

  run <- reticulate::import("pharmpy.tools.external.nlmixr.run", convert = FALSE)
  modeling <- reticulate::import("pharmpy.modeling", convert = FALSE)
  model <- modeling$load_example_model("pheno")

  writer <- if(reticulate::py_has_attr(run, "write_dataset")) {
    run$write_dataset
  } else {
    run$write_csv
  }

  tmp_dir <- withr::local_tempdir()
  writer(model, path = tmp_dir)
  ## upstream's collision guard must survive the filename fix
  expect_error(writer(model, path = tmp_dir), "exist")
  expect_silent(writer(model, path = tmp_dir, force = TRUE))
})

test_that("the package-level alias is patched too, and tolerates `strict=`", {
  skip_if_pharmpy_nlmixr_not_available()
  patch_pharmpy_nlmixr_results()

  ## `pharmpy/tools/external/nlmixr/__init__.py` does
  ## `from .run import parse_modelfit_results` — a separate binding, and the
  ## one `pharmpy.tools.read_modelfit_results()` resolves. Patching only
  ## `run` leaves that path on the buggy implementation.
  py <- reticulate::py_run_string("
import pharmpy.tools.external.nlmixr as _pkg
import pharmpy.tools.external.nlmixr.run as _run

_has_alias = hasattr(_pkg, 'parse_modelfit_results')
_alias_patched = (
    getattr(_pkg, 'parse_modelfit_results', None)
    is _run.parse_modelfit_results
)
", convert = TRUE)
  skip_if_not(isTRUE(py$`_has_alias`), "no package-level alias in this Pharmpy")
  expect_true(py$`_alias_patched`)

  ## `pharmpy.tools.external.results.parse_modelfit_results()` calls the
  ## backend as `(model, path, strict=strict)`; run.py's signature takes only
  ## two arguments, so the wrapper has to drop what it cannot pass on.
  run <- reticulate::import("pharmpy.tools.external.nlmixr.run", convert = TRUE)
  modeling <- reticulate::import("pharmpy.modeling", convert = FALSE)
  pathlib <- reticulate::import("pathlib", convert = FALSE)
  model <- modeling$load_example_model("pheno")

  expect_null(
    run$parse_modelfit_results(
      model, pathlib$Path(withr::local_tempdir()), strict = TRUE
    )
  )
})

test_that("parse_modelfit_results still returns None when there is no RDATA", {
  skip_if_pharmpy_nlmixr_not_available()
  patch_pharmpy_nlmixr_results()

  run <- reticulate::import("pharmpy.tools.external.nlmixr.run", convert = TRUE)
  modeling <- reticulate::import("pharmpy.modeling", convert = FALSE)
  pathlib <- reticulate::import("pathlib", convert = FALSE)
  model <- modeling$load_example_model("pheno")

  out <- run$parse_modelfit_results(model, pathlib$Path(withr::local_tempdir()))
  expect_null(out)
})
