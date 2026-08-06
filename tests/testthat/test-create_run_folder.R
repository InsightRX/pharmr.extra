test_that("creates new folder when it doesn't exist, returns correct path", {
  tmp_dir <- withr::local_tempdir()
  out <- create_run_folder(id = "test_run", path = tmp_dir, verbose = FALSE)
  
  expect_true(dir.exists(out))
  expect_equal(out, file.path(tmp_dir, "test_run"))
  expect_equal(basename(out), "test_run")
})

test_that("auto-generates run ID when id is NULL", {
  tmp_dir <- withr::local_tempdir()
  out <- create_run_folder(id = NULL, path = tmp_dir, verbose = FALSE)
  
  expect_true(dir.exists(out))
  expect_equal(basename(out), "run1")
})

test_that("auto-generates sequential run IDs", {
  tmp_dir <- withr::local_tempdir()
  # Create some existing run folders:
  dir.create(file.path(tmp_dir, "run1"))
  dir.create(file.path(tmp_dir, "run2"))
  out <- create_run_folder(id = NULL, path = tmp_dir, verbose = FALSE)
  
  expect_true(dir.exists(out))
  expect_equal(basename(out), "run3")
})

test_that("works with nested path structures", {
  tmp_dir <- withr::local_tempdir()
  nested_path <- file.path(tmp_dir, "level1", "level2")
  dir.create(nested_path, recursive = TRUE)
  
  out <- create_run_folder(id = "nested_run", path = nested_path, verbose = FALSE)
  
  expect_true(dir.exists(out))
  expect_equal(out, file.path(nested_path, "nested_run"))
})

test_that("creates missing parent folders when id is nested (e.g. run_sim regimens)", {
  tmp_dir <- withr::local_tempdir()
  ## id carrying a subfolder, as run_sim() uses for per-regimen output
  out <- create_run_folder(id = file.path("sim1", "regimen_2"),
                           path = tmp_dir, verbose = FALSE)

  expect_true(dir.exists(out))
  expect_equal(out, file.path(tmp_dir, "sim1", "regimen_2"))
})

test_that("errors when folder exists and force is FALSE", {
  tmp_dir <- withr::local_tempdir()
  existing_folder <- file.path(tmp_dir, "existing_run")
  dir.create(existing_folder)
  
  expect_error(
    create_run_folder(id = "existing_run", path = tmp_dir, force = FALSE, verbose = FALSE),
    "Run folder.*exists.*Use `force` to overwrite"
  )
})
