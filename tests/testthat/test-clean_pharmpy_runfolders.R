test_that("removes tool runfolders when remove = TRUE", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  dir.create(file.path(tmp_dir, "modelfit3"))
  
  # Verify folders exist:
  expect_true(dir.exists(file.path(tmp_dir, "modelfit1")))
  expect_true(dir.exists(file.path(tmp_dir, "modelfit2")))
  expect_true(dir.exists(file.path(tmp_dir, "modelfit3")))
  
  # Clean the folders:
  clean_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit", remove = TRUE)
  
  # Verify folders are removed:
  expect_false(dir.exists(file.path(tmp_dir, "modelfit1")))
  expect_false(dir.exists(file.path(tmp_dir, "modelfit2")))
  expect_false(dir.exists(file.path(tmp_dir, "modelfit3")))
})

test_that("preserves tool runfolders when remove = FALSE", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  
  # Verify folders exist:
  expect_true(dir.exists(file.path(tmp_dir, "modelfit1")))
  expect_true(dir.exists(file.path(tmp_dir, "modelfit2")))
  
  # Clean with remove = FALSE:
  clean_pharmpy_runfolders(folder = tmp_dir, tool = "modelfit", remove = FALSE)
  
  # Verify folders are preserved:
  expect_true(dir.exists(file.path(tmp_dir, "modelfit1")))
  expect_true(dir.exists(file.path(tmp_dir, "modelfit2")))
})

test_that("only removes folders matching the specified tool", {
  local_pharmr.extra_options()
  
  tmp_dir <- withr::local_tempdir()
  
  # Create folders for different tools:
  dir.create(file.path(tmp_dir, "modelfit1"))
  dir.create(file.path(tmp_dir, "modelfit2"))
  dir.create(file.path(tmp_dir, "search1"))
  dir.create(file.path(tmp_dir, "amd1"))
  
  # Clean only modelfit folders:
  clean_pharmpy_runfolders(
    folder = tmp_dir,
    tool = "modelfit",
    remove = TRUE
  )
  
  # modelfit folders should be removed:
  expect_false(dir.exists(file.path(tmp_dir, "modelfit1")))
  expect_false(dir.exists(file.path(tmp_dir, "modelfit2")))
  
  # Other tool folders should be preserved:
  expect_true(dir.exists(file.path(tmp_dir, "search1")))
  expect_true(dir.exists(file.path(tmp_dir, "amd1")))
})

test_that("works with id parameter", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  id_dir <- file.path(tmp_dir, "run1")
  dir.create(id_dir)
  dir.create(file.path(id_dir, "modelfit1"))
  dir.create(file.path(id_dir, "modelfit2"))
  
  # Verify folders exist:
  expect_true(dir.exists(file.path(id_dir, "modelfit1")))
  expect_true(dir.exists(file.path(id_dir, "modelfit2")))
  
  # Clean the folders:
  clean_pharmpy_runfolders(
    id = "run1",
    folder = tmp_dir,
    tool = "modelfit",
    remove = TRUE
  )
  
  # Verify folders are removed:
  expect_false(dir.exists(file.path(id_dir, "modelfit1")))
  expect_false(dir.exists(file.path(id_dir, "modelfit2")))
  
  # id directory should still exist:
  expect_true(dir.exists(id_dir))
})

test_that("preserves other folders and files in id directory", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  id_dir <- file.path(tmp_dir, "run1")
  dir.create(id_dir)
  dir.create(file.path(id_dir, "modelfit1"))
  dir.create(file.path(id_dir, "modelfit2"))
  dir.create(file.path(id_dir, "other_folder"))
  writeLines("test", file.path(id_dir, "model.mod"))
  
  # Clean modelfit folders:
  clean_pharmpy_runfolders(
    id = "run1",
    folder = tmp_dir,
    tool = "modelfit",
    remove = TRUE
  )
  
  # modelfit folders should be removed:
  expect_false(dir.exists(file.path(id_dir, "modelfit1")))
  expect_false(dir.exists(file.path(id_dir, "modelfit2")))
  
  # Other folders and files should be preserved:
  expect_true(dir.exists(id_dir))
  expect_true(dir.exists(file.path(id_dir, "other_folder")))
  expect_true(file.exists(file.path(id_dir, "model.mod")))
})

test_that("handles no matching tool folders", {
  local_pharmr.extra_options()
  
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "othertool1"))
  dir.create(file.path(tmp_dir, "somefolder"))
  
  # Should not error:
  expect_no_error(
    clean_pharmpy_runfolders(
      folder = tmp_dir,
      tool = "modelfit",
      remove = TRUE
    )
  )
  
  # Non-matching folders should be preserved:
  expect_true(dir.exists(file.path(tmp_dir, "othertool1")))
  expect_true(dir.exists(file.path(tmp_dir, "somefolder")))
})

test_that("handles empty directory gracefully", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  
  expect_no_error(
    clean_pharmpy_runfolders(
      folder = tmp_dir,
      tool = "modelfit",
      remove = TRUE
    )
  )
  expect_length(list.dirs(tmp_dir, recursive = FALSE), 0)
})
