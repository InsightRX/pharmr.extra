test_that("get_initial_estimates_from_data works for 1-compartment model", {
  # Create test data
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )
  
  # Get estimates
  result <- get_initial_estimates_from_data(test_data, n_cmt = 1)
  
  # Test results
  expect_type(result, "list")
  expect_named(result, c("V", "CL"))
  expect_true(all(result > 0))  # All parameters should be positive
  
  # Test approximate values (using known decay pattern)
  expect_equal(result$V, 1000/100, tolerance = 0.1)  # V = dose/Cmax
  expect_equal(result$CL, 1.98, tolerance = 0.2)  # Approximate half-life
})

test_that("get_initial_estimates_from_data works for 2-compartment model", {
  # Create test data
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )
  
  # Get estimates
  result <- get_initial_estimates_from_data(test_data, n_cmt = 2)
  
  # Test results
  expect_type(result, "list")
  expect_named(result, c("V", "CL", "QP1", "VP1"))
  expect_true(all(result > 0))  # All parameters should be positive
  
  # Test relationships between parameters
  expect_equal(result$QP1, result$CL)  # Q equals CL for initial estimate
  expect_equal(result$VP1, result$V * 2)  # VP1 is twice V
})

test_that("get_initial_estimates_from_data works for 3-compartment model", {
  # Create test data
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )
  
  # Get estimates
  result <- get_initial_estimates_from_data(test_data, n_cmt = 3)
  
  # Test results
  expect_type(result, "list")
  expect_named(result, c("V", "CL", "QP1", "VP1", "QP2", "VP2"))
  expect_true(all(result > 0))  # All parameters should be positive
  
  # Test relationships between parameters
  expect_equal(result$QP1, result$CL)
  expect_equal(result$QP2, result$CL)
  expect_equal(result$VP1, result$V * 2)
  expect_equal(result$VP2, result$V * 3)
})


## Individual data:
test_that("get_initial_estimates_from_data handles multiple subjects", {
  # Create test data with two subjects
  test_data <- data.frame(
    ID = rep(c(1, 2), each = 4),
    TIME = rep(c(0, 1, 4, 8), 2),
    DV = c(0, 100, 50, 25, 0, 120, 60, 30),
    EVID = rep(c(1, 0, 0, 0), 2),
    MDV = rep(c(1, 0, 0, 0), 2),
    AMT = rep(c(1000, 0, 0, 0), 2)
  )
  
  # Get estimates
  result <- get_initial_estimates_from_data(test_data, n_cmt = 1)
  
  # Test results
  expect_type(result, "list")
  expect_named(result, c("V", "CL"))
  expect_true(all(result > 0))
  
  # Test that results are averages of individual estimates
  expect_true(result$V > 1000/120 && result$V < 1000/100)  # Should be between individual estimates
})

test_that("get_initial_estimates_from_individual_data works with simple PK data", {
  # Create test data
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 4, 8),
    DV = c(0, 10, 7, 3, 1),
    MDV = c(1, 0, 0, 0, 0),
    EVID = c(1, 0, 0, 0, 0),
    AMT = c(100, 0, 0, 0, 0)
  )
  
  # Get estimates
  estimates <- get_initial_estimates_from_individual_data(test_data)
  
  # Test that we get the expected parameters
  expect_named(estimates, c("V", "CL"))
  
  # Test that values are positive
  expect_true(all(estimates > 0))
  
  # Test approximate values (allowing for some numerical tolerance)
  # V should be approximately dose/Cmax = 100/10 = 10
  expect_equal(estimates["V"], c(V = 10), tolerance = 0.1)
  
  # Calculate expected CL from the data
  # Using two timepoints (1h and 8h):
  # KEL = (ln(10) - ln(1))/(8-1) ≈ 0.329
  # Expected CL = KEL * V ≈ 3.29
  expect_equal(estimates["CL"], c(CL=3.29), tolerance = 0.5)
})

test_that("get_initial_estimates_from_individual_data handles missing data", {
  # Create test data with some missing observations
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 4, 8),
    DV =   c(0, NA, 7, 3, 1),
    MDV =  c(1, 1, 0, 0, 0),
    EVID = c(1, 0, 0, 0, 0),
    AMT =  c(100, 0, 0, 0, 0)
  )
  
  # Get estimates
  estimates <- get_initial_estimates_from_individual_data(test_data)
  
  # Test that we still get estimates
  expect_named(estimates, c("V", "CL"))
  expect_true(all(estimates > 0))
})

test_that("get_initial_estimates_from_data handles ltbs = TRUE", {
  # Same PK profile as 1-cmt test, but DV is log-transformed
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, log(100), log(50), log(25)),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )

  result_ltbs <- get_initial_estimates_from_data(test_data, n_cmt = 1, ltbs = TRUE)
  result_normal <- get_initial_estimates_from_data(
    data.frame(
      ID = c(1, 1, 1, 1), TIME = c(0, 1, 4, 8),
      DV = c(0, 100, 50, 25), EVID = c(1, 0, 0, 0),
      MDV = c(1, 0, 0, 0), AMT = c(1000, 0, 0, 0)
    ),
    n_cmt = 1
  )

  # Results should be identical to the non-ltbs version with back-transformed data
  expect_equal(result_ltbs$V, result_normal$V, tolerance = 1e-6)
  expect_equal(result_ltbs$CL, result_normal$CL, tolerance = 1e-6)
})

test_that("get_initial_estimates_from_individual_data handles insufficient data", {
  # Create test data with only one observation
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1),
    DV = c(0, 10),
    MDV = c(1, 0),
    EVID = c(1, 0),
    AMT = c(100, 0)
  )
  
  # Get estimates - should return empty
  estimates <- get_initial_estimates_from_individual_data(test_data)
  
  # Test that we get an empty result
  expect_equal(estimates, c(V = 2, CL = 0.2))
})

test_that("get_initial_estimates_from_individual_data handles two observations at the same timepoint", {
  # Two observations at identical times — lm() cannot estimate a slope, so
  # the function must fall back to crude estimation rather than returning NaN/NA.
  test_data <- data.frame(
    ID   = 1,
    TIME = c(0, 4, 4),
    DV   = c(0, 50, 55),
    MDV  = c(1, 0,  0),
    EVID = c(1, 0,  0),
    AMT  = c(100, 0, 0)
  )

  estimates <- get_initial_estimates_from_individual_data(test_data)

  expect_named(estimates, c("V", "CL"))
  expect_true(all(!is.na(estimates)), label = "estimates must not be NA with duplicate timepoints")
  expect_true(all(estimates > 0),    label = "estimates must be positive")
})

test_that("get_initial_estimates_from_individual_data handles absorption-phase only data (negative KEL fallback)", {
  # Only absorption-phase observations: concentrations are still rising,
  # so lm(log(DV) ~ TIME) yields a positive slope -> negative KEL.
  # The fix should fall back to log(max/min) / time-range, giving a positive CL.
  test_data <- data.frame(
    ID   = 1,
    TIME = c(0, 0.5, 1, 2),
    DV   = c(0, 5,   8, 10),   # still rising — no terminal phase
    MDV  = c(1, 0,   0,  0),
    EVID = c(1, 0,   0,  0),
    AMT  = c(100, 0, 0,  0)
  )

  estimates <- get_initial_estimates_from_individual_data(test_data)

  expect_named(estimates, c("V", "CL"))
  expect_true(estimates["CL"] > 0, label = "CL must be positive even with absorption-phase-only data")
  expect_true(estimates["V"]  > 0, label = "V must be positive")
})

test_that("get_initial_estimates_from_data works with CSV filename", {
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv(test_data, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result_file <- get_initial_estimates_from_data(tmp, n_cmt = 1)
  result_df   <- get_initial_estimates_from_data(test_data, n_cmt = 1)

  expect_type(result_file, "list")
  expect_named(result_file, c("V", "CL"))
  expect_equal(result_file$V,  result_df$V,  tolerance = 1e-6)
  expect_equal(result_file$CL, result_df$CL, tolerance = 1e-6)
})

test_that("get_initial_estimates_from_data errors on non-existent CSV file", {
  expect_error(
    get_initial_estimates_from_data("/nonexistent/data.csv"),
    "`data` file does not exist"
  )
})
