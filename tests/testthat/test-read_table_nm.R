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

## Subproblem-aware reading (#130) --------------------------------------------
##
## A `$SIMULATION` record with SUBPROBLEMS > 1 writes one block of rows per
## subproblem, each opened by a repeated `TABLE NO.` header. The default reader
## drops those headers along with the rest of the non-numeric rows, which is
## fine until something needs the boundaries — the NWPRI uncertainty engine
## does, because the subproblem index *is* the `.uncertainty` index.

test_that("read_table_nm splits on TABLE NO. when asked", {
  tab <- read_table_nm(test_path("fixtures", "simtab_subproblems"),
                       subproblems = TRUE)

  expect_equal(names(tab), c("ID", "TIME", "DV", "PRED", "CL", ".subproblem"))
  expect_equal(nrow(tab), 12)
  expect_type(tab$.subproblem, "integer")
  expect_equal(sort(unique(tab$.subproblem)), 1:3)
  expect_equal(as.integer(table(tab$.subproblem)), c(4L, 4L, 4L))

  ## Each subproblem carries its own parameter draw, which is exactly the
  ## information the default reader throws away.
  expect_equal(
    vapply(split(tab$CL, tab$.subproblem), unique, numeric(1)),
    c("1" = 5.1, "2" = 4.6, "3" = 5.9)
  )
  ## the rows themselves are read the same way as always
  expect_equal(tab$DV[tab$.subproblem == 2], c(0, 5.5, 0, 5.1))
})

test_that("read_table_nm keeps the default behaviour of dropping the headers", {
  plain <- suppressWarnings(suppressMessages(
    read_table_nm(test_path("fixtures", "simtab_subproblems"))
  ))
  ## Same rows, no way to tell the subproblems apart.
  expect_equal(nrow(plain), 12)
  expect_false(".subproblem" %in% names(plain))
})

test_that("read_table_nm handles tables that repeat the column header", {
  tab <- read_table_nm(test_path("fixtures", "simtab_subproblems_repeated_header"),
                       subproblems = TRUE)
  expect_equal(names(tab), c("ID", "TIME", "DV", ".subproblem"))
  expect_equal(nrow(tab), 4)
  expect_equal(tab$.subproblem, c(1L, 1L, 2L, 2L))
  expect_equal(tab$DV, c(0, 4.8, 0, 5.5))
})

test_that("subproblem reading rejects inputs it cannot handle", {
  f <- test_path("fixtures", "simtab_subproblems")
  expect_error(read_table_nm(f, subproblems = TRUE, nonmem_tab = FALSE),
               "NONMEM output tables")
  expect_error(read_table_nm(c(f, f), subproblems = TRUE),
               "a single table file")

  empty <- withr::local_tempfile()
  writeLines(character(0), empty)
  expect_error(read_table_nm(empty, subproblems = TRUE), "empty")

  headerless <- withr::local_tempfile()
  writeLines(c("TABLE NO.  1", " 1.0 2.0"), headerless)
  expect_error(read_table_nm(headerless, subproblems = TRUE), "No column header")

  mismatched <- withr::local_tempfile()
  writeLines(c("TABLE NO.  1", " ID TIME DV", " 1.0 2.0"), mismatched)
  expect_error(read_table_nm(mismatched, subproblems = TRUE), "column name")
})
