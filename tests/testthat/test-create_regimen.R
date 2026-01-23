test_that("basic oral regimen creation works", {
  reg <- create_regimen(
    dose = 500,
    interval = 12,
    n = 10,
    route = "oral"
  )
  
  expect_s3_class(reg, "dosing_regimen")
  expect_s3_class(reg, "data.frame")
  expect_equal(nrow(reg), 10)
  expect_equal(reg$dose, rep(500, 10))
  expect_equal(reg$route, rep("oral", 10))
  expect_equal(reg$t_inf, rep(0, 10))
  expect_equal(reg$interval, rep(12, 10))
  expect_equal(reg$time, seq(0, 108, by = 12))
})

test_that("oral regimen with different parameters works", {
  reg <- create_regimen(
    dose = 1000,
    interval = 24,
    n = 5,
    route = "oral"
  )
  
  expect_equal(nrow(reg), 5)
  expect_equal(reg$dose, rep(1000, 5))
  expect_equal(reg$interval, rep(24, 5))
  expect_equal(reg$time, c(0, 24, 48, 72, 96))
  expect_equal(reg$t_inf, rep(0, 5))
})

test_that("IV regimen with t_inf specified works", {
  reg <- create_regimen(
    dose = 200,
    interval = 8,
    n = 3,
    route = "iv",
    t_inf = 1
  )
  
  expect_equal(nrow(reg), 3)
  expect_equal(reg$dose, rep(200, 3))
  expect_equal(reg$route, rep("iv", 3))
  expect_equal(reg$t_inf, rep(1, 3))
  expect_equal(reg$time, c(0, 8, 16))
})

test_that("IV regimen without t_inf defaults to 0", {
  expect_message(
    reg <- create_regimen(
      dose = 100,
      interval = 12,
      n = 2,
      route = "iv"
    ),
    "assuming bolus dose"
  )
  
  expect_equal(reg$t_inf, rep(0, 2))
  expect_equal(reg$route, rep("iv", 2))
})

test_that("SC regimen without t_inf defaults to 0", {
  expect_message(
    reg <- create_regimen(
      dose = 50,
      interval = 24,
      n = 3,
      route = "sc"
    ),
    "assuming bolus dose"
  )
  
  expect_equal(reg$t_inf, rep(0, 3))
  expect_equal(reg$route, rep("sc", 3))
})

test_that("IM regimen without t_inf defaults to 0", {
  expect_message(
    reg <- create_regimen(
      dose = 75,
      interval = 6,
      n = 4,
      route = "im"
    ),
    "assuming bolus dose"
  )
  
  expect_equal(reg$t_inf, rep(0, 4))
  expect_equal(reg$route, rep("im", 4))
})

test_that("SC regimen with t_inf specified works", {
  reg <- create_regimen(
    dose = 50,
    interval = 24,
    n = 2,
    route = "sc",
    t_inf = 0.5
  )
  
  expect_equal(reg$t_inf, rep(0.5, 2))
  expect_equal(reg$route, rep("sc", 2))
})

test_that("IM regimen with t_inf specified works", {
  reg <- create_regimen(
    dose = 75,
    interval = 12,
    n = 2,
    route = "im",
    t_inf = 0.25
  )
  
  expect_equal(reg$t_inf, rep(0.25, 2))
  expect_equal(reg$route, rep("im", 2))
})

test_that("regimen with single dose works", {
  reg <- create_regimen(
    dose = 500,
    interval = 24,
    n = 1,
    route = "oral"
  )
  
  expect_equal(nrow(reg), 1)
  expect_equal(reg$time, 0)
  expect_equal(reg$dose, 500)
})

test_that("time sequence is correctly calculated", {
  reg <- create_regimen(
    dose = 100,
    interval = 6,
    n = 5,
    route = "oral"
  )
  
  expected_times <- c(0, 6, 12, 18, 24)
  expect_equal(reg$time, expected_times)
})

test_that("regimen has correct column names and structure", {
  reg <- create_regimen(
    dose = 500,
    interval = 12,
    n = 3,
    route = "oral"
  )
  
  expect_named(reg, c("dose", "time", "route", "t_inf", "interval"))
  expect_equal(ncol(reg), 5)
})

test_that("invalid route produces error", {
  expect_error(
    create_regimen(
      dose = 100,
      interval = 12,
      n = 2,
      route = "invalid_route"
    ),
    "should be one of"
  )
})

test_that("oral route ignores t_inf parameter", {
  reg <- create_regimen(
    dose = 500,
    interval = 12,
    n = 2,
    route = "oral",
    t_inf = 2
  )
  
  # t_inf should be 0 for oral, even if specified
  expect_equal(reg$t_inf, rep(0, 2))
})

test_that("regimen handles fractional doses", {
  reg <- create_regimen(
    dose = 12.5,
    interval = 8,
    n = 3,
    route = "oral"
  )
  
  expect_equal(reg$dose, rep(12.5, 3))
})

test_that("regimen handles fractional intervals", {
  reg <- create_regimen(
    dose = 100,
    interval = 8.5,
    n = 2,
    route = "oral"
  )
  
  expect_equal(reg$interval, rep(8.5, 2))
  expect_equal(reg$time, c(0, 8.5))
})

test_that("route matching works with unambiguous partial matches", {
  reg <- create_regimen(dose = 100, interval = 12, n = 1, route = "o")
  expect_equal(reg$route, "oral")
})
