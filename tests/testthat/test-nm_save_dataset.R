test_that("saves data.frame correctly for nonmem tool", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    DV = c(0, 10, 0, 15),
    AMT = c(100, 0, 100, 0),
    EVID = c(1, 0, 1, 0)
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  nm_save_dataset(dat, tmp, tool = "nonmem")
  expect_true(file.exists(tmp))
  
  # Read back and verify content:
  out <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(out, dat)
})

test_that("replaces NAs with '.' for nonmem tool", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    DV = c(0, 10, NA, 15),
    AMT = c(100, 0, 100, NA),
    EVID = c(1, 0, 1, 0),
    WT = c(70, NA, 75, 80)
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  nm_save_dataset(dat, tmp, tool = "nonmem")
  
  # Read back and verify NAs are replaced with "."
  out <- read.csv(tmp, stringsAsFactors = FALSE, na.strings = ".")
  expect_equal(out, dat)
  
  # Verify that "." appears in the raw file content:
  out <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(out$DV, c(0, 10, ".", 15))
  expect_equal(out$AMT, c(100, 0, 100, "."))
  expect_equal(out$WT, c(70, ".", 75, 80))
})

test_that("preserves NAs for nlmixr tool", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    DV = c(0, 10, NA, 15),
    AMT = c(100, 0, 100, NA),
    EVID = c(1, 0, 1, 0),
    WT = c(70, NA, 75, 80)
  )
  tmp <- withr::local_tempfile(fileext = ".csv")
  nm_save_dataset(dat, tmp, tool = "nlmixr")
  
  # Read back and verify NAs are present:
  out <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(out, dat)
})

test_that("errors on invalid tool argument", {
  dat <- data.frame(ID = 1, DV = 10)
  tmp <- withr::local_tempfile(fileext = ".csv")
  
  expect_error(
    nm_save_dataset(dat, tmp, tool = "invalid"),
    "must be one of"
  )
})
