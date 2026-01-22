test_that("returns correct ADVAN for 1-compartment IV model", {
  skip_on_ci()
  advan1 <- create_model(route = "iv", n_cmt = 1)
  result <- get_advan(advan1)
  expect_equal(result, 1L)
  expect_type(result, "integer")
})

test_that("returns correct ADVAN for 1-compartment oral model", {
  skip_on_ci()
  advan2 <- create_model(route = "oral", n_cmt = 1)
  result <- get_advan(advan2)
  expect_equal(result, 2L)
  expect_type(result, "integer")
})

test_that("returns correct ADVAN for 2-compartment IV model", {
  skip_on_ci()
  advan3 <- create_model(route = "iv", n_cmt = 2)
  result <- get_advan(advan3)
  expect_equal(result, 3L)
  expect_type(result, "integer")
})

test_that("returns correct ADVAN for 2-compartment oral model", {
  skip_on_ci()
  advan4 <- create_model(route = "oral", n_cmt = 2)
  result <- get_advan(advan4)
  expect_equal(result, 4L)
  expect_type(result, "integer")
})

test_that("returns correct ADVAN for 3-compartment IV model", {
  skip_on_ci()
  advan5 <- create_model(route = "iv", n_cmt = 3)
  result <- get_advan(advan5)
  expect_equal(result, 11L)
  expect_type(result, "integer")
})

test_that("returns correct ADVAN for Michaelis-Menten elimination model", {
  skip_on_ci()
  advan6 <- create_model(
    route = "oral", 
    n_cmt = 2,
    elimination = "michaelis-menten"
  )
  result <- get_advan(advan6)
  # Michaelis-Menten can result in ADVAN6 or ADVAN13 depending on implementation
  expect_true(result %in% c(6L, 13L))
  expect_type(result, "integer")
})
