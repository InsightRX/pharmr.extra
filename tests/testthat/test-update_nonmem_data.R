test_that("update_nonmem_data replaces the dataset path", {
  code <- "$PROB Test\n$DATA old.csv\n$INPUT ID TIME DV"
  out <- update_nonmem_data(code, "new.csv")
  expect_match(out, "\\$DATA new\\.csv")
  expect_false(grepl("old\\.csv", out))
})

test_that("update_nonmem_data preserves $DATA options (IGNORE, ACCEPT, REWIND, RECORDS)", {
  code <- "$PROB T\n$DATA old.csv IGNORE=@ ACCEPT=(DV.GT.0) REWIND RECORDS=1000\n$INPUT ID TIME DV"
  out <- update_nonmem_data(code, "new.csv")
  expect_match(out, "\\$DATA new\\.csv IGNORE=@ ACCEPT=\\(DV\\.GT\\.0\\) REWIND RECORDS=1000")
})

test_that("update_nonmem_data preserves other records and lines", {
  code <- c(
    "$PROB Test",
    "$DATA old.csv IGNORE=@",
    "$INPUT ID TIME DV AMT EVID",
    "$SUBROUTINES ADVAN1 TRANS2",
    "$THETA (0, 10)",
    "$OMEGA 0.1"
  )
  out <- update_nonmem_data(code, "new.csv")
  expect_length(out, length(code))
  expect_equal(out[-2], code[-2])
  expect_equal(out[[2]], "$DATA new.csv IGNORE=@")
})

test_that("update_nonmem_data preserves an inline comment on the $DATA line", {
  code <- "$PROB T\n$DATA old.csv IGNORE=@ ; this is the training data\n$INPUT ID TIME DV"
  out <- update_nonmem_data(code, "new.csv")
  expect_match(out, "\\$DATA new\\.csv IGNORE=@ ; this is the training data")
})

test_that("update_nonmem_data handles quoted existing paths", {
  code_dq <- "$PROB T\n$DATA \"old file.csv\" IGNORE=@\n$INPUT ID"
  out_dq <- update_nonmem_data(code_dq, "new.csv")
  expect_match(out_dq, "\\$DATA new\\.csv IGNORE=@")

  code_sq <- "$PROB T\n$DATA 'old file.csv' IGNORE=@\n$INPUT ID"
  out_sq <- update_nonmem_data(code_sq, "new.csv")
  expect_match(out_sq, "\\$DATA new\\.csv IGNORE=@")
})

test_that("update_nonmem_data quotes new path if it contains whitespace", {
  code <- "$PROB T\n$DATA old.csv IGNORE=@\n$INPUT ID"
  out <- update_nonmem_data(code, "/path with spaces/new.csv")
  expect_match(out, "\\$DATA '/path with spaces/new\\.csv' IGNORE=@", fixed = FALSE)
})

test_that("update_nonmem_data preserves leading whitespace and tabs on the $DATA line", {
  code <- "$PROB T\n   $DATA\told.csv\tIGNORE=@\n$INPUT ID"
  out <- update_nonmem_data(code, "new.csv")
  ## Leading whitespace preserved
  expect_match(out, "\n   \\$DATA\tnew\\.csv\tIGNORE=@")
})

test_that("update_nonmem_data accepts vector input and returns a vector", {
  code <- c("$PROB T", "$DATA old.csv IGNORE=@", "$INPUT ID TIME DV")
  out <- update_nonmem_data(code, "new.csv")
  expect_type(out, "character")
  expect_length(out, 3)
  expect_equal(out[[2]], "$DATA new.csv IGNORE=@")
})

test_that("update_nonmem_data only updates the first $DATA record", {
  ## A second $DATA-like token in a comment or elsewhere should not be
  ## replaced.
  code <- c(
    "$PROB Test",
    "$DATA old.csv IGNORE=@",
    "; $DATA not_a_real_record.csv",
    "$INPUT ID TIME DV"
  )
  out <- update_nonmem_data(code, "new.csv")
  expect_equal(out[[2]], "$DATA new.csv IGNORE=@")
  expect_equal(out[[3]], "; $DATA not_a_real_record.csv")
})

test_that("update_nonmem_data accepts case-insensitive and abbreviated $DAT", {
  out1 <- update_nonmem_data("$prob t\n$data old.csv IGNORE=@\n$input ID", "new.csv")
  expect_match(out1, "\\$data new\\.csv IGNORE=@", ignore.case = TRUE)

  out2 <- update_nonmem_data("$PROB T\n$DAT old.csv IGNORE=@\n$INPUT ID", "new.csv")
  expect_match(out2, "\\$DAT new\\.csv IGNORE=@")
})

test_that("update_nonmem_data errors on missing $DATA record or invalid input", {
  expect_error(
    update_nonmem_data("$PROB T\n$INPUT ID TIME DV", "new.csv"),
    "No \\$DATA record found"
  )
  expect_error(update_nonmem_data(character(0), "new.csv"), "non-empty")
  expect_error(update_nonmem_data("$DATA old.csv", ""), "non-empty")
  expect_error(update_nonmem_data("$DATA old.csv", c("a", "b")), "single")
  expect_error(update_nonmem_data("$DATA old.csv", NA_character_), "non-empty")
})
