## Fake Pharmpy search results: the tool is inferred from the class name.
mock_search_results <- function(
  class = "pharmpy.tools.modelsearch.results.ModelSearchResults",
  final_results = list(ofv = 100)
) {
  structure(
    list(final_results = final_results, final_model = "final_model"),
    class = class
  )
}

local_search_folder <- function(subfolders, .local_envir = parent.frame()) {
  id <- withr::local_tempdir(.local_envir = .local_envir)
  for (f in subfolders) dir.create(file.path(id, f))
  id
}

test_that("get_final_results_from_search attaches fit info from the last run folder", {
  id <- local_search_folder(c("modelsearch1", "modelsearch2", "modelsearch10", "other"))
  captured <- NULL
  local_mocked_bindings(
    attach_fit_info = function(fit, model, fit_folder, ...) {
      captured <<- list(fit = fit, model = model, fit_folder = fit_folder)
      "fit_with_info"
    }
  )
  res <- get_final_results_from_search(id, mock_search_results())
  expect_equal(res, "fit_with_info")
  expect_equal(captured$fit, list(ofv = 100))
  expect_equal(captured$model, "final_model")
  expect_equal(captured$fit_folder, file.path(id, "modelsearch10", "models", "final"))
})

test_that("get_final_results_from_search uses an explicitly supplied tool", {
  id <- local_search_folder(c("iivsearch1", "iivsearch3", "modelsearch5"))
  captured <- NULL
  local_mocked_bindings(
    attach_fit_info = function(fit, model, fit_folder, ...) {
      captured <<- fit_folder
      NULL
    }
  )
  get_final_results_from_search(
    id,
    mock_search_results(class = "some.other.Class"),
    tool = "iivsearch"
  )
  expect_equal(captured, file.path(id, "iivsearch3", "models", "final"))
})

test_that("get_final_results_from_search aborts on unrecognized results objects", {
  expect_error(
    get_final_results_from_search("run1", mock_search_results(class = "data.frame")),
    "don't recognize"
  )
})

test_that("get_final_results_from_search aborts when no final results exist", {
  expect_error(
    get_final_results_from_search("run1", mock_search_results(final_results = NULL)),
    "no final results"
  )
})
