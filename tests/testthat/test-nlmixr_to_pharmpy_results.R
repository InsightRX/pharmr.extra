test_that("nlmixr-shaped results convert to native Pharmpy ModelfitResults", {
  skip_if_not(reticulate::py_module_available("pandas"))
  skip_if_not(reticulate::py_module_available("pharmpy.workflows.results"))

  fit <- list(
    ofv = 1.2,
    parameter_estimates = c(POP_CL = 1.1, POP_V = 20),
    standard_errors = c(POP_CL = 0.1, POP_V = 2),
    relative_standard_errors = c(POP_CL = 0.09, POP_V = 0.1),
    minimization_successful = TRUE,
    covstep_successful = TRUE,
    termination_cause = "ok",
    warnings = character(0),
    predictions = data.frame(PRED = c(1, 2)),
    residuals = data.frame(CWRES = c(0.1, -0.1))
  )
  class(fit) <- c("nlmixr2_modelfit_results", "list")

  out <- as_native_pharmpy_modelfit_results(fit)

  expect_s3_class(out, "pharmpy.workflows.results.ModelfitResults")
  expect_equal(reticulate::py_to_r(out$ofv), 1.2)
  expect_equal(reticulate::py_to_r(out$parameter_estimates$loc["POP_CL"]), 1.1)
})

test_that("native Pharmpy results are returned unchanged", {
  skip_if_not(reticulate::py_module_available("pharmpy.workflows.results"))

  workflows <- reticulate::import("pharmpy.workflows.results", convert = FALSE)
  res <- workflows$ModelfitResults(ofv = 1)

  expect_identical(as_native_pharmpy_modelfit_results(res), res)
})
