test_that("updates parameters from fit object with fix = FALSE", {
  skip_on_ci()
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  out <- update_parameters(mod, fit, fix = FALSE, verbose = FALSE)
  params_df <- out$parameters$to_dataframe()
  
  expect_false(all(params_df$fix))
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("updates parameters from fit object with fix = TRUE", {
  skip_on_ci()
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  out <- update_parameters(mod, fit, fix = TRUE, verbose = FALSE)
  params_df <- out$parameters$to_dataframe()
  
  expect_true(all(params_df$fix))
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})
