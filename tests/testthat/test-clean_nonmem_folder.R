test_that("removes blacklisted temporary files from NONMEM folder", {
  tmp_dir <- withr::local_tempdir()
  
  # Create some blacklisted files:
  blacklisted_files <- c("FCON", "FDATA", "FDATA.csv", "FMSG", "FORIG", 
                         "FREPL", "FREPORT", "FSIZES", "compile.lnk")
  for (f in blacklisted_files) {
    writeLines("test", file.path(tmp_dir, f))
  }
  
  # Verify files exist:
  expect_true(all(file.exists(file.path(tmp_dir, blacklisted_files))))
  
  # Clean the folder:
  clean_nonmem_folder(tmp_dir)
  
  # Verify blacklisted files are removed:
  expect_false(any(file.exists(file.path(tmp_dir, blacklisted_files))))
})

test_that("preserves other files", {
  tmp_dir <- withr::local_tempdir()
  # Create some non-blacklisted files (typical NONMEM output files):
  preserved_files <- c("run1.lst", "run1.mod", "run1.csv", "data.csv", 
                       "myoutput.txt", "results.xlsx")
  for (f in preserved_files) {
    writeLines("test", file.path(tmp_dir, f))
  }
  # Create one blacklisted file:
  writeLines("test", file.path(tmp_dir, "FCON"))
  
  # Verify non-blacklisted files are preserved and blacklist removed:
  clean_nonmem_folder(tmp_dir)
  expect_true(all(file.exists(file.path(tmp_dir, preserved_files))))
  expect_false(file.exists(file.path(tmp_dir, "FCON")))
})

test_that("handles empty directory gracefully", {
  tmp_dir <- withr::local_tempdir()
  expect_length(list.files(tmp_dir), 0)
  
  expect_no_error(clean_nonmem_folder(tmp_dir))
  expect_length(list.files(tmp_dir), 0)
})

test_that("removes Pharmpy's hidden scratch folders", {
  tmp_dir <- withr::local_tempdir()
  ## `.modeldb` and `.pharmpy` are folders, and hidden, so removing them needs
  ## both `dir(all.files = TRUE)` and a recursive unlink.
  dir.create(file.path(tmp_dir, ".modeldb", "models"), recursive = TRUE)
  writeLines("cached", file.path(tmp_dir, ".modeldb", "models", "m1.mod"))
  dir.create(file.path(tmp_dir, ".pharmpy"))
  writeLines("state", file.path(tmp_dir, ".pharmpy", "state.json"))
  writeLines("test", file.path(tmp_dir, "run.lst"))

  clean_nonmem_folder(tmp_dir)

  expect_false(dir.exists(file.path(tmp_dir, ".modeldb")))
  expect_false(dir.exists(file.path(tmp_dir, ".pharmpy")))
  expect_true(file.exists(file.path(tmp_dir, "run.lst")))
})

test_that("leaves other hidden entries alone", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, ".datasets"))
  writeLines("ID,TIME,DV", file.path(tmp_dir, ".datasets", "d1.csv"))
  writeLines("keep", file.path(tmp_dir, ".Rprofile"))

  clean_nonmem_folder(tmp_dir)

  expect_true(file.exists(file.path(tmp_dir, ".datasets", "d1.csv")))
  expect_true(file.exists(file.path(tmp_dir, ".Rprofile")))
})
