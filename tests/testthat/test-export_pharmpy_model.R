test_that("exporting and importing a pharmpy model object works", {
  mod1 <- create_model()
  tmp <- paste0(tempfile(), ".rds")
  export_pharmpy_model(mod1, tmp)  
  tmp2 <- import_pharmpy_model(tmp)
  expect_equal(
    class(tmp2),
    c("pharmpy.model.external.nonmem.model.Model", "pharmpy.model.model.Model", 
      "pharmpy.internals.immutable.Immutable", "abc.ABC", "python.builtin.object"
    )
  )
})

test_that("importing a non-pharmpy model object throws an informative error", {
  tmp <- paste0(tempfile(), ".rds")
  saveRDS(list(abc = "def"), tmp)
  expect_error(
    import_pharmpy_model(tmp),
    "The object you are trying to import is not a Pharmpy model object"
  )
})

test_that("exporting a non-pharmpy model object throws an informative error", {
  tmp <- paste0(tempfile(), ".rds")
  saveRDS(list(abc = "def"), tmp)
  expect_error(
    export_pharmpy_model(list(abc = "def"), tmp),
    "The object you are trying to export is not a Pharmpy model object"
  )
})
