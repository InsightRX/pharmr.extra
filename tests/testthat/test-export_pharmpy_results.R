test_that("export and import pharmpy results works", {
  ## create a mock object
  model1 <- create_model()
  res <- list(
    final_model = model1,
    models = list(
      model1
    ),
    model = model1,
    summary_models = list(),
    summary_tool = list(),
    summary_errors = list(),
    final_results = list()
  )
  class(res) <- c("pharmpy.tools.modelsearch.tool.ModelSearchResults")
  tmp <- paste0(tempfile(), ".rds")
  export_pharmpy_results(res, tmp)
  tmp2 <- import_pharmpy_results(tmp)
  expect_equal(
    names(tmp2),
    c("final_model", "models", "summary_models", "summary_tool", 
     "summary_errors", "final_results")
  )
})

test_that("export pharmpy results throws an error when not a Pharmpy results object", {
  tmp <- paste0(tempfile(), ".rds")
  expect_error(
    export_pharmpy_results(list(abc = "def"), tmp),
    "Object class not recognized, cannot export."
  )
})

test_that("import pharmpy results throws an error when not a Pharmpy results object", {
  tmp <- paste0(tempfile(), ".rds")
  saveRDS(list(abc = "def"), tmp)
  expect_error(
    import_pharmpy_results(tmp),
    "Object class not recognized, cannot import."
  )
})

test_that("import pharmpy results throws an appropriate error when trying to load a Pharmpy model object", {
  model1 <- create_model()
  tmp <- paste0(tempfile(), ".rds")
  export_pharmpy_model(model1, tmp)
  expect_error(
    import_pharmpy_results(tmp),
    "Object looks like a Pharmpy model object. Please use"
  )
})
