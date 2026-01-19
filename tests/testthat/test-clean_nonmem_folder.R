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
