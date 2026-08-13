test_that("resolve_output_file() anchors relative names to `path`", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)

  expect_identical(
    pharmr.extra:::resolve_output_file("run1.rds", tmp),
    file.path(tmp, "run1.rds")
  )
})

test_that("resolve_output_file() creates `path` if it does not exist", {
  tmp <- file.path(tempfile(), "nested")
  on.exit(unlink(dirname(tmp), recursive = TRUE), add = TRUE)

  out <- pharmr.extra:::resolve_output_file("run1.rds", tmp)
  expect_identical(out, file.path(tmp, "run1.rds"))
  expect_true(dir.exists(tmp))
})

test_that("resolve_output_file() leaves absolute paths untouched", {
  expect_identical(
    pharmr.extra:::resolve_output_file("/abs/where/fit.rds", tempdir()),
    "/abs/where/fit.rds"
  )
  expect_identical(
    pharmr.extra:::resolve_output_file("~/fit.rds", tempdir()),
    "~/fit.rds"
  )
  expect_identical(
    pharmr.extra:::resolve_output_file("C:\\tmp\\fit.rds", tempdir()),
    "C:\\tmp\\fit.rds"
  )
})

test_that("resolve_output_file() falls back to the bare name without a path", {
  expect_identical(pharmr.extra:::resolve_output_file("run1.rds", NULL), "run1.rds")
  expect_identical(pharmr.extra:::resolve_output_file("run1.rds", NA), "run1.rds")
  expect_identical(pharmr.extra:::resolve_output_file("run1.rds", ""), "run1.rds")
})
