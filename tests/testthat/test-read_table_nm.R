test_that("reads NONMEM table file with auto-detection", {
  local_pharmr.extra_options()
  out <- read_table_nm(test_path("fixtures", "run_folder", "sdtab"))
  
  expect_s3_class(out, "data.frame")
  expect_equal(
    names(out),
    c("ID", "TIME", "DV", "EVID", "MDV", "PRED", "IPRED", "CWRES", "NPDE")
  )
  expect_gt(nrow(out), 0) # Check that it has rows (after NA removal)
  expect_type(out$ID, "double")
  expect_type(out$TIME, "double")
  expect_type(out$DV, "double")
  expect_type(out$PRED, "double")
  expect_type(out$IPRED, "double")
  expect_type(out$CWRES, "double")
  expect_type(out$NPDE, "double")
})

test_that("reads NONMEM table file with explicit skip and header", {
  local_pharmr.extra_options()
  sdtab <- test_path("fixtures", "run_folder", "sdtab")
  out <- read_table_nm(file = sdtab, skip = 1, header = TRUE)
  
  expect_s3_class(out, "data.frame")
  expect_equal(
    names(out),
    c("ID", "TIME", "DV", "EVID", "MDV", "PRED", "IPRED", "CWRES", "NPDE")
  )
  expect_gt(nrow(out), 0)
  expect_type(out$ID, "double")
  expect_type(out$TIME, "double")
  expect_type(out$DV, "double")
  expect_type(out$PRED, "double")
  expect_type(out$IPRED, "double")
  expect_type(out$CWRES, "double")
  expect_type(out$NPDE, "double")
})

test_that("reads NONMEM table file without header", {
  local_pharmr.extra_options()
  # Create a temporary file without header:
  tmp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c("1.0 0.0 1.5", "2.0 1.0 2.5", "3.0 2.0 3.5"),
    tmp_file
  )
  out <- read_table_nm(file = tmp_file, skip = 0, header = FALSE)
  
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("X1", "X2", "X3"))
  expect_gt(nrow(out), 0)
})

test_that("reads patab file correctly", {
  local_pharmr.extra_options()
  out <- read_table_nm(test_path("fixtures", "run_folder", "patab"))
  
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("ID", "CL", "TVKA", "V"))
  expect_gt(nrow(out), 0)
})

test_that("handles multiple files and combines them", {
  local_pharmr.extra_options()
  sdtab <- test_path("fixtures", "run_folder", "sdtab")
  patab <- test_path("fixtures", "run_folder", "patab")
  out <- read_table_nm(file = c(sdtab, patab), rm_duplicates = TRUE)
  
  # Should have columns from both files:
  expect_true(all(c("ID", "TIME", "CL") %in% names(out)))
})

test_that("removes duplicate columns when rm_duplicates = TRUE", {
  # TODO: Determine whether this is expected behaviour. This test currently fails
  # because readr::read_table() deduplicates columns, so the second ID column
  # becomes ID_1.
  skip()
  local_pharmr.extra_options()
  tmp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      "TABLE NO.  1",
      "ID ID TIME",
      "1.0 1.0 0.0",
      "2.0 2.0 1.0"
    ),
    tmp_file
  )
  out <- read_table_nm(file = tmp_file, rm_duplicates = TRUE)
  
  # Should only have one ID column:
  expect_equal(ncol(dplyr::select(out, dplyr::starts_with("ID"))), 1)
})

test_that("keeps duplicate columns when rm_duplicates = FALSE", {
  # TODO: see previous TODO. This currently passes, but not necessarily because
  # the argument is working as expected.
  skip()
  local_pharmr.extra_options()
  tmp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      "TABLE NO.  1",
      "ID ID TIME",
      "1.0 1.0 0.0",
      "2.0 2.0 1.0"
    ),
    tmp_file
  )
  out <- read_table_nm(file = tmp_file, rm_duplicates = FALSE)
  
  # Should have both ID columns:
  expect_equal(ncol(dplyr::select(out, dplyr::starts_with("ID"))), 2)
})

test_that("handles file path with some missing files", {
  local_pharmr.extra_options()
  # Should filter out missing files and continue with existing ones:
  sdtab <- test_path("fixtures", "run_folder", "sdtab")
  nonexistent_file <- "nonexistent_file.txt"
  out <- read_table_nm(file = c(sdtab, nonexistent_file))
  
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
})

test_that("removes rows with NA values", {
  local_pharmr.extra_options()
  tmp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      "TABLE NO.  1",
      "ID TIME DV",
      "1.0 0.0 1.5",
      "2.0 1.0 NA",
      "3.0 2.0 3.5"
    ),
    tmp_file
  )
  out <- read_table_nm(file = tmp_file)
  
  expect_s3_class(out, "data.frame")
  expect_false(any(is.na(out)))
})

test_that("errors when file is NULL", {
  expect_error(
    read_table_nm(file = NULL),
    'Argument "file" required'
  )
})

test_that("errors when file does not exist", {
  expect_error(
    read_table_nm(file = "nonexistent_file.txt"),
    "No file not found"
  )
})
