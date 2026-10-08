# Tests for the Zephyr reporting helpers in tools/zephyr/zephyr_lib.R.
#
# The library lives under tools/ (excluded from the built package), so it is
# only present when tests run against the source tree (e.g. testthat::test_local
# or in CI). When testing an installed package the file is absent and these
# tests skip.

zephyr_lib <- testthat::test_path("..", "..", "tools", "zephyr", "zephyr_lib.R")

skip_if_no_zephyr_lib <- function() {
  testthat::skip_if_not(file.exists(zephyr_lib), "zephyr_lib.R not on source tree")
}

local_zephyr_lib <- function(env = parent.frame()) {
  skip_if_no_zephyr_lib()
  source(zephyr_lib, local = env)
}

test_that("extract_zephyr_key pulls the key out of a test name", {
  local_zephyr_lib()
  expect_equal(extract_zephyr_key("[PHAI-T23] builds a model"), "PHAI-T23")
  expect_equal(extract_zephyr_key("does PHAI-T9 work"), "PHAI-T9")
  expect_true(is.na(extract_zephyr_key("no key here")))
})

test_that("parse_junit_results extracts status, key and time per testcase", {
  local_zephyr_lib()
  res <- parse_junit_results(test_path("fixtures", "zephyr_junit_example.xml"))

  expect_equal(nrow(res), 5)
  expect_setequal(res$status, c("pass", "fail", "skip"))

  pass_2cmt <- res[res$name == "[PHAI-T23] builds a 2-cmt model", ]
  expect_equal(pass_2cmt$status, "fail")
  expect_equal(pass_2cmt$key, "PHAI-T23")

  oral <- res[res$name == "[PHAI-T24] handles oral route", ]
  expect_equal(oral$status, "skip")

  untagged <- res[res$name == "untagged helper test", ]
  expect_true(is.na(untagged$key))
})

test_that("aggregate_executions collapses tests to one execution per key", {
  local_zephyr_lib()
  res <- parse_junit_results(test_path("fixtures", "zephyr_junit_example.xml"))
  agg <- aggregate_executions(res)

  # Untagged test is dropped; T23, T24, T25 remain.
  expect_setequal(agg$key, c("PHAI-T23", "PHAI-T24", "PHAI-T25"))

  # T23 has one pass + one fail -> overall fail, times summed (0.5 + 1.5 = 2.0s).
  t23 <- agg[agg$key == "PHAI-T23", ]
  expect_equal(t23$status, "fail")
  expect_equal(t23$execution_time_ms, 2000L)
  expect_equal(t23$n_tests, 2L)

  # T24 only skipped -> skip.
  expect_equal(agg[agg$key == "PHAI-T24", ]$status, "skip")

  # T25 single pass.
  t25 <- agg[agg$key == "PHAI-T25", ]
  expect_equal(t25$status, "pass")
  expect_equal(t25$execution_time_ms, 3000L)
})

test_that("zephyr_status_name maps internal statuses to Zephyr labels", {
  local_zephyr_lib()
  expect_equal(zephyr_status_name("pass"), "Pass")
  expect_equal(zephyr_status_name("fail"), "Fail")
  expect_equal(zephyr_status_name("skip"), "Not Executed")
})
