test_that("aborts with class pharmr_extra_sim_failed when run folder is empty", {
  tmp <- withr::local_tempdir()
  expect_error(
    abort_on_failed_sim("regimen A", tmp),
    class = "pharmr_extra_sim_failed"
  )
})

test_that("includes snippet starting at NONMEM error marker in run.lst", {
  tmp <- withr::local_tempdir()
  writeLines(
    c(
      "preamble line",
      " AN ERROR WAS FOUND IN THE CONTROL STATEMENTS.",
      " (DATA ITEM 5): SS NOT DEFINED",
      "trailing"
    ),
    file.path(tmp, "run.lst")
  )
  err <- tryCatch(
    abort_on_failed_sim("regimen B", tmp),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_match(msg, "AN ERROR WAS FOUND IN THE CONTROL STATEMENTS")
  expect_match(msg, "SS NOT DEFINED")
  expect_match(msg, "regimen B")
})

test_that("falls back to .lst tail when no known marker is present", {
  tmp <- withr::local_tempdir()
  lines <- c(paste0("line", 1:50), "FINAL TAIL LINE")
  writeLines(lines, file.path(tmp, "run.lst"))
  err <- tryCatch(
    abort_on_failed_sim("regimen C", tmp),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "FINAL TAIL LINE")
})

test_that("braces in NONMEM output do not break glue interpolation", {
  tmp <- withr::local_tempdir()
  writeLines(
    c("PRED EXIT CODE = 1", "weird {token}", "x{y}"),
    file.path(tmp, "run.lst")
  )
  err <- tryCatch(
    abort_on_failed_sim("regimen D", tmp),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_match(msg, "PRED EXIT CODE")
  expect_match(msg, "\\{token\\}", perl = TRUE)
})

test_that("falls back to stderr when run.lst is absent", {
  tmp <- withr::local_tempdir()
  writeLines(c("nmfe error: cannot find data file"), file.path(tmp, "stderr"))
  err <- tryCatch(
    abort_on_failed_sim("regimen E", tmp),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "cannot find data file")
})

test_that("includes NM-TRAN warnings from the top of run.lst", {
  tmp <- withr::local_tempdir()
  writeLines(
    c(
      "$PROBLEM sim",
      " WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1",
      "",
      " (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.",
      "",
      " (DATA WARNING   5) RECORD    2, DATA ITEM   3: SOMETHING ODD",
      " CONTINUATION OF DATA WARNING",
      "",
      " (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.",
      "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.5.0",
      " (WARNING  99) THIS IS NOT AN NM-TRAN WARNING",
      paste0("data record ", 1:40),
      " PROGRAM TERMINATED BY OBJ",
      " TIME DATA ITEM IS LESS THAN PREVIOUS"
    ),
    file.path(tmp, "run.lst")
  )
  err <- tryCatch(
    abort_on_failed_sim("regimen F", tmp),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_match(msg, "NM-TRAN warnings")
  expect_match(msg, "INFERS THAT THE DATA ARE SINGLE-SUBJECT")
  expect_match(msg, "CONTINUATION OF DATA WARNING")
  expect_match(msg, "TIME DATA ITEM IS LESS THAN PREVIOUS")
  expect_false(grepl("NOT AN NM-TRAN WARNING", msg))
  ## deduplicated
  expect_equal(
    lengths(regmatches(msg, gregexpr("SINGLE-SUBJECT", msg))),
    1
  )
  ## warnings come before the error snippet
  expect_lt(
    regexpr("SINGLE-SUBJECT", msg),
    regexpr("TIME DATA ITEM", msg)
  )
})

test_that("NM-TRAN warnings are capped and not repeated from the snippet", {
  w <- sprintf(" (WARNING  %d) MESSAGE NUMBER %d", 1:15, 1:15)
  out <- extract_nmtran_warnings(w, max_warnings = 10)
  expect_length(out, 10)
  expect_equal(attr(out, "n_total"), 15)

  tmp <- withr::local_tempdir()
  writeLines(
    c(w, "1NONLINEAR MIXED EFFECTS MODEL PROGRAM", paste0("line", 1:40)),
    file.path(tmp, "run.lst")
  )
  err <- tryCatch(
    abort_on_failed_sim("regimen G", tmp),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "and 5 more warning")

  tmp2 <- withr::local_tempdir()
  writeLines(
    c(
      " MESSAGE ISSUED FROM NMTRAN",
      " (WARNING  3) ONLY IN SNIPPET"
    ),
    file.path(tmp2, "run.lst")
  )
  err2 <- tryCatch(
    abort_on_failed_sim("regimen H", tmp2),
    error = function(e) e
  )
  msg2 <- conditionMessage(err2)
  expect_false(grepl("NM-TRAN warnings", msg2))
  expect_match(msg2, "ONLY IN SNIPPET")
})

test_that("extract_nmtran_warnings returns empty for no warnings", {
  expect_length(extract_nmtran_warnings(character(0)), 0)
  expect_length(extract_nmtran_warnings(c("a", "b")), 0)
})
