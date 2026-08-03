test_that("nm_table_format_width() reads the field width of a format spec", {
  expect_equal(pharmr.extra:::nm_table_format_width("sF11.0"), 11L)
  expect_equal(pharmr.extra:::nm_table_format_width("F11.0"), 11L)
  expect_equal(pharmr.extra:::nm_table_format_width("s1PE15.8"), 15L)
  expect_equal(pharmr.extra:::nm_table_format_width(",1PE11.4"), 11L)
  expect_equal(pharmr.extra:::nm_table_format_width("sG9.3"), 9L)
  expect_true(is.na(pharmr.extra:::nm_table_format_width(NULL)))
  expect_true(is.na(pharmr.extra:::nm_table_format_width("nonsense")))
})

test_that("check_table_formats() accepts formats NONMEM can write", {
  ## Verified against NONMEM 7.5.1: each column occupies width(FORMAT) + 1
  ## characters, so IDFORMAT must not be wider than FORMAT (11 by default).
  expect_no_error(pharmr.extra:::check_table_formats("sF11.0"))
  expect_no_error(pharmr.extra:::check_table_formats("sF9.0"))
  expect_no_error(pharmr.extra:::check_table_formats(NULL, "sF9.0"))
  expect_no_error(pharmr.extra:::check_table_formats("sF15.0", "s1PE15.8"))
  ## Unparseable specs are left to NONMEM to reject
  expect_no_error(pharmr.extra:::check_table_formats("something"))
})

test_that("check_table_formats() rejects an ID column wider than the field", {
  expect_error(pharmr.extra:::check_table_formats("sF12.0"), "wider than")
  expect_error(pharmr.extra:::check_table_formats("sF16.0", "s1PE15.8"), "wider than")
  expect_error(pharmr.extra:::check_table_formats(11), "single string")
  expect_error(pharmr.extra:::check_table_formats(c("sF11.0", "sF11.0")), "single string")
})
