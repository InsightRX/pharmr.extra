# get_fit_info() --------------------------------------------------------------
test_that("returns correct structure with all expected fields", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  run_folder <- test_path("fixtures", "run_folder")
  out <- get_fit_info(fit, run_folder)
  
  # Check class:
  expect_s3_class(out, "pharmpy_fit_info")
  expect_s3_class(out, "list")
  
  # Check all expected fields are present:
  expected_fields <- c(
    "ofv", "condition_number", "shrinkage", "eta_bar",
    "iterations", "function_evaluations", "parameter_estimates",
    "standard_errors", "relative_standard_errors", "runtime", "run_info"
  )
  expect_named(out, expected_fields)
  
  # Check structure of nested fields:
  expect_type(out$shrinkage, "list")
  expect_named(out$shrinkage, c("eta", "ebv", "eps"))
  
  expect_type(out$runtime, "list")
  expect_named(out$runtime, c("estimation", "total"))
  
  expect_type(out$run_info, "list")
  expect_named(
    out$run_info,
    c(
      "minimization_successful", "covstep_successful",
      "termination_cause", "warnings", "significant_digits"
    )
  )
})

test_that("extracts fit information correctly from fit object", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  run_folder <- test_path("fixtures", "run_folder")
  out <- get_fit_info(fit, run_folder)
  
  expect_equal(out$ofv, fit$ofv)
  expect_equal(out$iterations, length(fit$ofv_iterations))
  expect_equal(out$function_evaluations, fit$function_evaluations)
  expect_equal(out$parameter_estimates, fit$parameter_estimates)
  expect_equal(out$standard_errors, fit$standard_errors)
  expect_equal(out$relative_standard_errors, fit$relative_standard_errors)
  expect_equal(out$runtime$estimation, fit$estimation_runtime)
  expect_equal(out$runtime$total, fit$runtime_total)
})

test_that("converts boolean values to yes/no strings in run_info", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  run_folder <- test_path("fixtures", "run_folder")
  out <- get_fit_info(fit, run_folder)
  
  # Check boolean conversions:
  expect_type(out$run_info$minimization_successful, "character")
  expect_type(out$run_info$covstep_successful, "character")
  expect_true(out$run_info$minimization_successful %in% c("yes", "no"))
  expect_true(out$run_info$covstep_successful %in% c("yes", "no"))
  
  # Verify conversion logic:
  if (fit$minimization_successful) {
    expect_equal(out$run_info$minimization_successful, "yes")
  } else {
    expect_equal(out$run_info$minimization_successful, "no")
  }
  
  if (fit$covstep_successful) {
    expect_equal(out$run_info$covstep_successful, "yes")
  } else {
    expect_equal(out$run_info$covstep_successful, "no")
  }
})

test_that("extracts condition number correctly", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  run_folder <- test_path("fixtures", "run_folder")
  out <- get_fit_info(fit, run_folder)
  
  expect_equal(out$condition_number, get_condition_number_for_fit(fit))
})

test_that("extracts shrinkage summary from lst file", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  run_folder <- test_path("fixtures", "run_folder")
  lst_file <- file.path(run_folder, "run.lst")
  out <- get_fit_info(fit, run_folder)
  
  expect_equal(out$shrinkage, get_shrinkage_summary(path = lst_file, fit = fit))
})

test_that("handles missing lst file gracefully", {
  local_pharmr.extra_options()
  fit <- readRDS(test_path("fixtures", "fit.rds"))
  tmp_dir <- withr::local_tempdir()
  out <- get_fit_info(fit, tmp_dir)
  
  # When lst file doesn't exist, shrinkage should return empty list:
  expect_type(out$shrinkage, "list")
  expect_equal(out$shrinkage, list())
})
