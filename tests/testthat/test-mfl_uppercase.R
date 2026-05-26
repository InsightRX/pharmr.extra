test_that("detect_lowercase_identifiers returns lowercase tokens only", {
  expect_equal(
    pharmr.extra:::detect_lowercase_identifiers("COVARIATE([CL,V], [wt,age], EXP)"),
    c("wt", "age")
  )
})

test_that("detect_lowercase_identifiers returns empty when all uppercase", {
  expect_equal(
    pharmr.extra:::detect_lowercase_identifiers("COVARIATE([CL,V], [WT,AGE], EXP)"),
    character(0)
  )
})

test_that("detect_lowercase_identifiers returns empty for NULL or non-character", {
  expect_equal(pharmr.extra:::detect_lowercase_identifiers(NULL), character(0))
  expect_equal(pharmr.extra:::detect_lowercase_identifiers(123), character(0))
})

test_that("detect_lowercase_identifiers deduplicates repeated tokens", {
  expect_equal(
    pharmr.extra:::detect_lowercase_identifiers("COVARIATE([cl], [wt], LIN); COVARIATE([cl], [wt], POW)"),
    c("cl", "wt")
  )
})

test_that("detect_lowercase_identifiers flags mixed-case tokens", {
  expect_equal(
    pharmr.extra:::detect_lowercase_identifiers("COVARIATE([CL], [Weight], EXP)"),
    "Weight"
  )
})
