# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

test_that("ruvsearch aborts with clear message when residuals are missing", {
  mod <- create_model()
  # Simulate a results object with no residuals (e.g. from a search tool or SAEM run)
  mock_results <- structure(list(residuals = NULL), class = "ModelfitResults")
  expect_error(
    call_pharmpy_tool(
      id = "test_ruvsearch",
      model = mod,
      results = mock_results,
      tool = "ruvsearch"
    ),
    "residuals"
  )
})
