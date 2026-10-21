write_nm_model <- function(code) {
  f <- withr::local_tempfile(fileext = ".mod", .local_envir = parent.frame())
  writeLines(code, f)
  f
}

test_that("nm_update_dataset replaces the dataset path and keeps options", {
  f <- write_nm_model(c(
    "$PROBLEM Test",
    "$INPUT ID TIME DV",
    "$DATA old.csv IGNORE=@ ACCEPT=(EVID.EQ.0)",
    "$PK",
    "CL=THETA(1)"
  ))
  nm_update_dataset(f, "new.csv", overwrite = TRUE)
  out <- readLines(f)
  data_line <- grep("^\\$DATA", out, value = TRUE)
  expect_equal(data_line, "$DATA new.csv IGNORE=@ ACCEPT=(EVID.EQ.0)")
  expect_false(any(grepl("old.csv", out)))
  expect_true("CL=THETA(1)" %in% out)
})

test_that("nm_update_dataset handles a dataset on a continuation line", {
  f <- write_nm_model(c(
    "$PROBLEM Test",
    "$INPUT ID TIME DV",
    "$DATA",
    "old.csv IGNORE=@",
    "$PK",
    "CL=THETA(1)"
  ))
  nm_update_dataset(f, "new.csv", overwrite = TRUE)
  out <- readLines(f)
  expect_true("new.csv IGNORE=@" %in% out)
  expect_false(any(grepl("old.csv", out)))
})

test_that("nm_update_dataset refuses to overwrite without `overwrite = TRUE`", {
  f <- write_nm_model(c("$PROBLEM Test", "$DATA old.csv", "$INPUT ID"))
  expect_error(nm_update_dataset(f, "new.csv"), "already exists")
})
