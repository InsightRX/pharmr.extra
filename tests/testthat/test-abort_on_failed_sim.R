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
