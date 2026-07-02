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
  expect_named(estimates, c("V", "CL", "weight"))
  
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
  expect_named(estimates, c("V", "CL", "weight"))
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
  expect_equal(estimates, c(V = 2, CL = 0.1, weight = 0.001))
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

  expect_named(estimates, c("V", "CL", "weight"))
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

  expect_named(estimates, c("V", "CL", "weight"))
  expect_true(estimates["CL"] > 0, label = "CL must be positive even with absorption-phase-only data")
  expect_true(estimates["V"]  > 0, label = "V must be positive")
})

test_that("get_initial_estimates_from_data downweighs single-observation subjects", {
  # Subject 1 has a well-characterized PK profile (multiple samples) -> reliable
  # V and CL estimates. Subject 2 has only a single observation -> crude guess.
  # The pooled estimate should be dominated by subject 1, not the crude guess
  # from subject 2, because single-observation subjects are downweighted.
  rich <- data.frame(
    ID   = 1,
    TIME = c(0, 1, 4, 8),
    DV   = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV  = c(1, 0, 0, 0),
    AMT  = c(1000, 0, 0, 0)
  )
  sparse <- data.frame(
    ID   = 2,
    TIME = c(0, 4),
    DV   = c(0, 50),
    EVID = c(1, 0),
    MDV  = c(1, 0),
    AMT  = c(1000, 0)
  )
  combined <- rbind(rich, sparse)

  result_rich_only <- get_initial_estimates_from_data(rich, n_cmt = 1)
  result_sparse_only <- get_initial_estimates_from_data(sparse, n_cmt = 1)
  result_combined <- get_initial_estimates_from_data(combined, n_cmt = 1)

  # Sanity check: the crude single-point estimate differs substantially from
  # the well-characterized one (otherwise this test proves nothing).
  expect_true(abs(result_sparse_only$CL - result_rich_only$CL) > 0.5)

  # Combined estimate should be very close to the rich-only estimate, because
  # the single-observation subject carries weight ~0.001.
  expect_equal(result_combined$V,  result_rich_only$V,  tolerance = 0.01)
  expect_equal(result_combined$CL, result_rich_only$CL, tolerance = 0.01)
})

test_that("get_initial_estimates_from_individual_data assigns low weight for single-observation subjects", {
  # With only one observation we cannot estimate a slope; the function returns
  # a crude guess that should be flagged with a very low weight so it is
  # effectively ignored when richer data is available.
  sparse <- data.frame(
    ID   = 1,
    TIME = c(0, 4),
    DV   = c(0, 50),
    EVID = c(1, 0),
    MDV  = c(1, 0),
    AMT  = c(1000, 0)
  )
  rich <- data.frame(
    ID   = 1,
    TIME = c(0, 1, 4, 8),
    DV   = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV  = c(1, 0, 0, 0),
    AMT  = c(1000, 0, 0, 0)
  )

  est_sparse <- get_initial_estimates_from_individual_data(sparse)
  est_rich   <- get_initial_estimates_from_individual_data(rich)

  expect_true("weight" %in% names(est_sparse))
  expect_true("weight" %in% names(est_rich))
  expect_lt(est_sparse[["weight"]], est_rich[["weight"]])
  expect_lt(est_sparse[["weight"]], 0.01)
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

test_that("get_initial_estimates_from_data works when AMT is character", {
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c("1000", "0", "0", "0"),
    stringsAsFactors = FALSE
  )

  result <- get_initial_estimates_from_data(test_data, n_cmt = 1)

  expect_type(result, "list")
  expect_named(result, c("V", "CL"))
  expect_true(all(result > 0))
  expect_equal(result$V, 10, tolerance = 0.1)
})

test_that("get_initial_estimates_from_data works when AMT is factor", {
  test_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = factor(c("1000", "0", "0", "0"))
  )

  result <- get_initial_estimates_from_data(test_data, n_cmt = 1)

  expect_type(result, "list")
  expect_named(result, c("V", "CL"))
  expect_true(all(result > 0))
  expect_equal(result$V, 10, tolerance = 0.1)
})

test_that("get_initial_estimates_from_data gives same result regardless of AMT column type", {
  base_data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0),
    AMT = c(1000, 0, 0, 0)
  )

  result_numeric   <- get_initial_estimates_from_data(base_data, n_cmt = 1)
  result_character <- get_initial_estimates_from_data(
    transform(base_data, AMT = as.character(AMT)), n_cmt = 1
  )
  result_factor    <- get_initial_estimates_from_data(
    transform(base_data, AMT = as.factor(AMT)), n_cmt = 1
  )

  expect_equal(result_numeric$V,  result_character$V,  tolerance = 1e-6)
  expect_equal(result_numeric$CL, result_character$CL, tolerance = 1e-6)
  expect_equal(result_numeric$V,  result_factor$V,     tolerance = 1e-6)
  expect_equal(result_numeric$CL, result_factor$CL,    tolerance = 1e-6)
})

test_that("get_initial_estimates_from_data is robust to EVID=2 rows (other-event/reset rows)", {
  # NONMEM datasets often contain EVID=2 rows (other-event, e.g. covariate
  # changes). They are not dose events and must not increment the per-subject
  # dose counter. Previously, cumsum(EVID) inflated the counter on EVID=2 rows
  # so the observation lookup couldn't find a matching AMT, the resulting V/CL
  # were length-0, and unlist() silently dropped them — producing a degenerate
  # `c(weight = ...)` vector. The pooled result then carried a `weight`
  # parameter, which downstream pharmpy rejected as `POP_weight`.
  with_evid2 <- data.frame(
    ID   = c(1, 1, 1, 1, 1),
    TIME = c(0, 0.5, 1, 4, 8),
    DV   = c(0, 0,   100, 50, 25),
    EVID = c(1, 2,   0,   0,  0),  # EVID=2 row sits between dose and obs
    MDV  = c(1, 1,   0,   0,  0),
    AMT  = c(1000, 0, 0,  0,  0)
  )
  without_evid2 <- data.frame(
    ID   = c(1, 1, 1, 1),
    TIME = c(0, 1, 4, 8),
    DV   = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV  = c(1, 0, 0, 0),
    AMT  = c(1000, 0, 0, 0)
  )

  result_with    <- get_initial_estimates_from_data(with_evid2,    n_cmt = 1)
  result_without <- get_initial_estimates_from_data(without_evid2, n_cmt = 1)

  expect_named(result_with, c("V", "CL"))
  expect_false("weight" %in% names(result_with))
  expect_equal(result_with$V,  result_without$V,  tolerance = 1e-6)
  expect_equal(result_with$CL, result_without$CL, tolerance = 1e-6)
})

test_that("get_initial_estimates_from_individual_data returns V/CL/weight even when no dose precedes observations", {
  # Subject has observations before any EVID=1 dose record (e.g. baseline /
  # pre-dose samples in a screening period). With cumsum(EVID==1), those obs
  # are bucketed under dose_nr = 0, for which no AMT row exists. The function
  # must return a full V/CL/weight vector (with NA V/CL) rather than a
  # partial one that unlist() would collapse to just `weight`.
  test_data <- data.frame(
    ID   = 1,
    TIME = c(0, 1, 4),
    DV   = c(5, 7, 6),
    EVID = c(0, 0, 0),
    MDV  = c(0, 0, 0),
    AMT  = c(0, 0, 0)
  )

  estimates <- get_initial_estimates_from_individual_data(test_data)

  expect_named(estimates, c("V", "CL", "weight"))
  expect_true(is.na(estimates[["V"]]))
  expect_true(is.na(estimates[["CL"]]))
  expect_equal(estimates[["weight"]], 0)
})

test_that("get_initial_estimates_from_data never returns a 'weight' parameter, even with degenerate subjects", {
  # Mix of a richly sampled subject and a subject whose observations sit
  # before any dose (no AMT match). The pooled result must contain only V and
  # CL — any spurious 'weight' entry here breaks set_initial_estimates with
  # ValueError: Parameters not found in model: ['POP_weight'].
  rich <- data.frame(
    ID   = 1,
    TIME = c(0, 1, 4, 8),
    DV   = c(0, 100, 50, 25),
    EVID = c(1, 0, 0, 0),
    MDV  = c(1, 0, 0, 0),
    AMT  = c(1000, 0, 0, 0)
  )
  pre_dose_only <- data.frame(
    ID   = 2,
    TIME = c(0, 1, 4),
    DV   = c(5, 7, 6),
    EVID = c(0, 0, 0),
    MDV  = c(0, 0, 0),
    AMT  = c(0, 0, 0)
  )

  result <- get_initial_estimates_from_data(rbind(rich, pre_dose_only), n_cmt = 1)

  expect_named(result, c("V", "CL"))
  expect_false("weight" %in% names(result))
  expect_true(all(unlist(result) > 0))
})
