test_that("finds tool runfolders in specified folder", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  dir.create(file.path(tmp_dir, "modelfit10"))
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")

  # Should be ordered numerically:
  expect_equal(out, c("modelfit1", "modelfit2", "modelfit10"))
})

test_that("works with different tool names", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "search1"))
  dir.create(file.path(tmp_dir, "search2"))
  dir.create(file.path(tmp_dir, "amd1"))
  dir.create(file.path(tmp_dir, "amd5"))
  out_search <- get_pharmpy_runfolders(folder = tmp_dir, tool = "search")
  out_amd <- get_pharmpy_runfolders(folder = tmp_dir, tool = "amd")
  
  expect_equal(out_search, c("search1", "search2"))
  expect_equal(out_amd, c("amd1", "amd5"))
})

test_that("works with id parameter", {
  tmp_dir <- withr::local_tempdir()
  # Create tool folders inside id directory:
  id_dir <- file.path(tmp_dir, "model123")
  dir.create(id_dir)
  dir.create(file.path(id_dir, "modelfit1"))
  dir.create(file.path(id_dir, "modelfit2"))
  out <- get_pharmpy_runfolders(
    id = "model123", folder = tmp_dir, tool = "modelfit"
  )
  
  expect_equal(out, c("modelfit1", "modelfit2"))
})

test_that("works with id and folder parameters together", {
  tmp_dir <- withr::local_tempdir()
  id_dir <- file.path(tmp_dir, "test_model")
  dir.create(id_dir)
  dir.create(file.path(id_dir, "simulation1"))
  dir.create(file.path(id_dir, "simulation2"))
  out <- get_pharmpy_runfolders(
    id = "test_model", folder = tmp_dir, tool = "simulation"
  )
  
  expect_equal(out, c("simulation1", "simulation2"))
})

test_that("works without folder parameter (uses getwd)", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  dir.create("modelfit1")
  dir.create("modelfit2")
  out <- get_pharmpy_runfolders(tool = "modelfit")
  
  expect_equal(out, c("modelfit1", "modelfit2"))
})

test_that("filters tool folders correctly by pattern", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  dir.create(file.path(tmp_dir, "othertool1"))
  dir.create(file.path(tmp_dir, "modelfit"))
  dir.create(file.path(tmp_dir, "modelfitabc"))
  dir.create(file.path(tmp_dir, "abcmodelfit1"))
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  # Should only return modelfit1 and modelfit2 (pattern: ^modelfit[0-9]+?$):
  expect_equal(out, c("modelfit1", "modelfit2"))
})

test_that("orders tool folders numerically", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit10"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit20"))
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  # Should be ordered numerically, not lexicographically:
  expect_equal(out, c("modelfit1", "modelfit2", "modelfit10", "modelfit20"))
})

test_that("handles single digit and multi-digit numbers correctly", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit9"))
  dir.create(file.path(tmp_dir, "modelfit10"))
  dir.create(file.path(tmp_dir, "modelfit99"))
  dir.create(file.path(tmp_dir, "modelfit100"))
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  expect_equal(out, c("modelfit1", "modelfit9", "modelfit10", "modelfit99", "modelfit100"))
})

test_that("returns empty character vector when no matching folders exist", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "othertool1"))
  dir.create(file.path(tmp_dir, "somefolder"))
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  expect_type(out, "character")
  expect_length(out, 0)
})

test_that("returns empty character vector when folder is empty", {
  tmp_dir <- withr::local_tempdir()
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  expect_type(out, "character")
  expect_length(out, 0)
})

test_that("ignores subdirectories (non-recursive)", {
  tmp_dir <- withr::local_tempdir()
  
  # Create tool folder at root level
  dir.create(file.path(tmp_dir, "modelfit1"))
  
  # Create nested tool folder (should be ignored)
  nested_dir <- file.path(tmp_dir, "modelfit1", "modelfit2")
  dir.create(nested_dir, recursive = TRUE)
  
  out <- get_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit")
  
  # Should only return modelfit1, not the nested modelfit2
  expect_equal(out, "modelfit1")
})
