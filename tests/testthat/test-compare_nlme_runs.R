test_that("finds and compares run folders", {
  local_pharmr.extra_options()
  # Create multiple run folders using fixtures:
  tmp_dir <- withr::local_tempdir()
  fixture_mod <- test_path("fixtures", "run_folder", "run.mod")
  fixture_lst <- test_path("fixtures", "run_folder", "run.lst")
  for (i in 1:3) {
    run_folder <- file.path(tmp_dir, paste0("run", i))
    dir.create(run_folder)
    file.copy(fixture_mod, file.path(run_folder, "run.mod"))
    file.copy(fixture_lst, file.path(run_folder, "run.lst"))
    # Small delay to ensure different creation times
    Sys.sleep(0.1)
  }
  
  # Compare the last 3 runs:
  out <- compare_nlme_runs(folder = tmp_dir, filter = "run", n = 3)
  
  # Check that out is returned:
  expect_type(out, "list")
  expect_length(out$info_comb, 3)
  expect_true("info_comb" %in% names(out))
  expect_true("par_comb" %in% names(out))
})

test_that("selects last n runs by creation time", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  fixture_mod <- test_path("fixtures", "run_folder", "run.mod")
  fixture_lst <- test_path("fixtures", "run_folder", "run.lst")
  for (i in 1:5) {
    run_folder <- file.path(tmp_dir, paste0("run", i))
    dir.create(run_folder)
    file.copy(fixture_mod, file.path(run_folder, "run.mod"))
    file.copy(fixture_lst, file.path(run_folder, "run.lst"))
    Sys.sleep(0.1)
  }
  
  # Compare last 2 runs (run4, run5):
  out <- compare_nlme_runs(folder = tmp_dir, filter = "run", n = 2)
  
  expect_length(out$info_comb, 2)
  expect_true("run5" %in% out$info_comb[[1]])
  expect_true("run4" %in% out$info_comb[[2]])
})

test_that("filters folders correctly", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  fixture_mod <- test_path("fixtures", "run_folder", "run.mod")
  fixture_lst <- test_path("fixtures", "run_folder", "run.lst")
  for (i in 1:2) {
    run_folder <- file.path(tmp_dir, paste0("run", i))
    dir.create(run_folder)
    file.copy(fixture_mod, file.path(run_folder, "run.mod"))
    file.copy(fixture_lst, file.path(run_folder, "run.lst"))
    Sys.sleep(0.1)
  }
  
  # Create a folder that doesn't match the filter:
  dir.create(file.path(tmp_dir, "other_folder"))
  
  # Should only find run folders
  out <- compare_nlme_runs(folder = tmp_dir, filter = "run", n = 3)
  
  expect_length(out$info_comb, 2)
  expect_true("run2" %in% out$info_comb[[1]])
  expect_true("run1" %in% out$info_comb[[2]])
})

test_that("can save info to csv", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  fixture_mod <- test_path("fixtures", "run_folder", "run.mod")
  fixture_lst <- test_path("fixtures", "run_folder", "run.lst")
  for (i in 1:2) {
    run_folder <- file.path(tmp_dir, paste0("run", i))
    dir.create(run_folder)
    file.copy(fixture_mod, file.path(run_folder, "run.mod"))
    file.copy(fixture_lst, file.path(run_folder, "run.lst"))
    Sys.sleep(0.1)
  }
  
  # Save info to csv:
  info_file <- withr::local_file(file.path(tmp_dir, "info.csv"))
  out <- compare_nlme_runs(
    folder = tmp_dir, 
    filter = "run", 
    n = 2,
    save_info = info_file
  )
  
  # Check that file was created and is readable:
  expect_true(file.exists(info_file))
  info_data <- read.csv(info_file, row.names = 1)
  expect_type(info_data, "list")
})

test_that("can save parameters to csv", {
  local_pharmr.extra_options()
  tmp_dir <- withr::local_tempdir()
  fixture_mod <- test_path("fixtures", "run_folder", "run.mod")
  fixture_lst <- test_path("fixtures", "run_folder", "run.lst")
  for (i in 1:2) {
    run_folder <- file.path(tmp_dir, paste0("run", i))
    dir.create(run_folder)
    file.copy(fixture_mod, file.path(run_folder, "run.mod"))
    file.copy(fixture_lst, file.path(run_folder, "run.lst"))
    Sys.sleep(0.1)
  }
  
  # Save parameters to csv:
  par_file <- withr::local_file(file.path(tmp_dir, "parameters.csv"))
  out <- compare_nlme_runs(
    folder = tmp_dir, 
    filter = "run", 
    n = 2,
    save_parameters = par_file
  )
  
  # Check that file was created and is readable:
  expect_true(file.exists(par_file))
  par_data <- read.csv(par_file, row.names = 1)
  expect_type(par_data, "list")
})
