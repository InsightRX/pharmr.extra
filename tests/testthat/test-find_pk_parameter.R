test_that("returns parameter when it exists as-is in model", {
  skip_on_ci()
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    WT = 70,
    CRCL = 80
  )
  mod_oral <- create_model(route = "oral", data = dat, verbose = FALSE)
  expect_equal(find_pk_parameter("CL", mod_oral), "CL")
  expect_equal(find_pk_parameter("V", mod_oral), "V")

  mod_iv <- create_model(route = "iv", data = dat, verbose = FALSE)
  expect_equal(find_pk_parameter("CL", mod_iv), "CL")
  expect_equal(find_pk_parameter("V", mod_iv), "V")
})
