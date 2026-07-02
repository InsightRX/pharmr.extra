test_that("no-op when default .ext exists and is non-empty", {
  d <- withr::local_tempdir()
  writeLines("content", file.path(d, "run.ext"))

  expect_false(ensure_default_ext_file(d, "run.mod", verbose = FALSE))
  expect_equal(readLines(file.path(d, "run.ext")), "content")
})

test_that("copies the lone non-default .ext to <stem>.ext when default is missing", {
  d <- withr::local_tempdir()
  writeLines("psn-content", file.path(d, "psn.ext"))

  expect_true(ensure_default_ext_file(d, "run.mod", verbose = FALSE))
  expect_true(file.exists(file.path(d, "run.ext")))
  expect_equal(readLines(file.path(d, "run.ext")), "psn-content")
  ## source is left alone
  expect_equal(readLines(file.path(d, "psn.ext")), "psn-content")
})

test_that("overwrites empty default .ext when a non-default one has content", {
  d <- withr::local_tempdir()
  file.create(file.path(d, "run.ext"))  # 0 bytes
  writeLines("psn-content", file.path(d, "psn.ext"))

  expect_true(ensure_default_ext_file(d, "run.mod", verbose = FALSE))
  expect_equal(readLines(file.path(d, "run.ext")), "psn-content")
})

test_that("ignores empty non-default .ext files", {
  d <- withr::local_tempdir()
  file.create(file.path(d, "psn.ext"))  # 0 bytes

  expect_false(ensure_default_ext_file(d, "run.mod", verbose = FALSE))
  expect_false(file.exists(file.path(d, "run.ext")))
})

test_that("does nothing when no .ext files at all", {
  d <- withr::local_tempdir()
  expect_false(ensure_default_ext_file(d, "run.mod", verbose = FALSE))
  expect_false(file.exists(file.path(d, "run.ext")))
})

test_that("warns and does nothing when multiple non-default .ext files exist", {
  d <- withr::local_tempdir()
  writeLines("a", file.path(d, "psn.ext"))
  writeLines("b", file.path(d, "other.ext"))

  ## cli alerts go through message(), not warning()
  expect_message(
    res <- ensure_default_ext_file(d, "run.mod", verbose = TRUE),
    "Multiple"
  )
  expect_false(res)
  expect_false(file.exists(file.path(d, "run.ext")))
})

test_that("aborts if the underlying file.copy fails", {
  skip_on_os("windows")  # chmod semantics differ on Windows
  d <- withr::local_tempdir()
  writeLines("psn-content", file.path(d, "psn.ext"))
  ## Make the destination unwritable by removing write perms on the folder
  Sys.chmod(d, mode = "0500")
  withr::defer(Sys.chmod(d, mode = "0700"))  # restore so tempdir can be cleaned

  expect_error(
    ensure_default_ext_file(d, "run.mod", verbose = FALSE),
    "Failed to copy"
  )
})

test_that("derives default name from the model stem", {
  d <- withr::local_tempdir()
  writeLines("psn-content", file.path(d, "psn.ext"))

  expect_true(ensure_default_ext_file(d, "mymodel.mod", verbose = FALSE))
  expect_true(file.exists(file.path(d, "mymodel.ext")))
  expect_false(file.exists(file.path(d, "run.ext")))
})
