test_that("attaches model, tables, and fit info to fit object", {
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  run_folder <- test_path("fixtures", "run_folder")
  out <- attach_fit_info(fit = fit, model = mod, fit_folder = run_folder)
  
  # fit object data is preserved:
  expect_equal(out$ofv, fit$ofv)
  expect_equal(out$parameter_estimates, fit$parameter_estimates)
  
  # model is attached as attribute:
  expect_true("model" %in% names(attributes(out)))
  expect_equal(attr(out, "model"), mod)
  
  # tables are attached as attribute:
  expect_true("tables" %in% names(attributes(out)))
  expect_type(attr(out, "tables"), "list")
  
  # fit info is attached as attribute (when not sim model):
  expect_true("info" %in% names(attributes(out)))
  expect_s3_class(attr(out, "info"), "pharmpy_fit_info")
})

test_that("attaches model and tables but not fit info for sim models", {
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  run_folder <- test_path("fixtures", "run_folder")
  out <- attach_fit_info(
    fit = fit, 
    model = mod, 
    fit_folder = run_folder,
    is_sim_model = TRUE
  )
  
  # model is attached:
  expect_true("model" %in% names(attributes(out)))
  expect_equal(attr(out, "model"), mod)
  
  # tables are attached:
  expect_true("tables" %in% names(attributes(out)))
  expect_type(attr(out, "tables"), "list")
  
  # fit info is NOT attached for sim models:
  expect_false("info" %in% names(attributes(out)))
})
