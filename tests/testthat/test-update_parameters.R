test_that("updates parameters from fit object with fix = FALSE", {
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  out <- update_parameters(mod, fit, fix = FALSE, verbose = FALSE)
  params_df <- out$parameters$to_dataframe()
  
  expect_false(all(params_df$fix))
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("updates parameters from fit object with fix = TRUE", {
  local_pharmr.extra_options()
  mod <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  out <- update_parameters(mod, fit, fix = TRUE, verbose = FALSE)
  params_df <- out$parameters$to_dataframe()

  expect_true(all(params_df$fix))
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("update_parameters() handles a raw nlmixr2 fit object", {
  local_pharmr.extra_options()
  mod <- pharmr::create_basic_pk_model(administration = "oral")
  mod <- pharmr::convert_model(mod, "nlmixr")

  ## A minimal stand-in for an nlmixr2 fit: parFixedDf for THETAs + residual
  ## error, omega for ETAs (block CL/VC + diagonal MAT). We only need the
  ## slots update_parameters() reads, so we tag the object with the right
  ## class and skip running an actual fit (nlmixr2 may not be installed).
  fake_raw_fit <- structure(
    list(
      parFixedDf = data.frame(
        Estimate = c(2.5, 22.0, 0.4, 0.15, NA, NA, NA),
        row.names = c("POP_CL", "POP_VC", "POP_MAT", "sigma",
                      "ETA_CL", "ETA_VC", "ETA_MAT")
      ),
      omega = matrix(
        c(0.05, 0.02, 0,
          0.02, 0.06, 0,
          0,    0,    0.04),
        nrow = 3,
        dimnames = list(c("ETA_CL", "ETA_VC", "ETA_MAT"),
                        c("ETA_CL", "ETA_VC", "ETA_MAT"))
      )
    ),
    class = c("nlmixr2FitData", "nlmixr2FitCore", "list")
  )

  out <- update_parameters(mod, fake_raw_fit, fix = FALSE)
  params <- out$parameters$to_dataframe()

  ## Population parameters and the residual error were applied
  expect_equal(params["POP_CL", "value"], 2.5)
  expect_equal(params["POP_VC", "value"], 22.0)
  expect_equal(params["POP_MAT", "value"], 0.4)
  expect_equal(params["sigma", "value"], 0.15)

  ## Both diagonal and off-diagonal omega entries were applied — the
  ## off-diagonal name uses pharmpy's `IIV_X_IIV_Y` convention.
  expect_equal(params["IIV_CL", "value"], 0.05)
  expect_equal(params["IIV_VC", "value"], 0.06)
  expect_equal(params["IIV_MAT", "value"], 0.04)
  expect_equal(params["IIV_CL_IIV_VC", "value"], 0.02)
})

test_that("update_parameters() with fix=TRUE on a raw nlmixr2 fit fixes block elements", {
  local_pharmr.extra_options()
  mod <- pharmr::create_basic_pk_model(administration = "oral")
  mod <- pharmr::convert_model(mod, "nlmixr")

  fake_raw_fit <- structure(
    list(
      parFixedDf = data.frame(
        Estimate = c(2.5, 22.0, 0.4, 0.15, NA, NA, NA),
        row.names = c("POP_CL", "POP_VC", "POP_MAT", "sigma",
                      "ETA_CL", "ETA_VC", "ETA_MAT")
      ),
      omega = matrix(
        c(0.05, 0.02, 0,
          0.02, 0.06, 0,
          0,    0,    0.04),
        nrow = 3,
        dimnames = list(c("ETA_CL", "ETA_VC", "ETA_MAT"),
                        c("ETA_CL", "ETA_VC", "ETA_MAT"))
      )
    ),
    class = c("nlmixr2FitData", "nlmixr2FitCore", "list")
  )

  out <- update_parameters(mod, fake_raw_fit, fix = TRUE)
  params <- out$parameters$to_dataframe()

  ## The block off-diagonal must also be fixed — not just the diagonal terms.
  expect_true(params["IIV_CL_IIV_VC", "fix"])
  expect_true(params["IIV_CL", "fix"])
  expect_true(params["IIV_VC", "fix"])
  expect_true(params["sigma", "fix"])
})
