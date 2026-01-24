test_that("works with valid model code and dataset", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  # Recreate model from list:
  mod_obj <- list(code = mod$code, dataset = mod$dataset)
  out <- create_pharmpy_model_from_list(mod_obj)
  
  # Data will be different, but the rest of the code should be equal:
  expect_equal(
    stringr::str_remove(out$code, "\\$DATA .+([\\/a-zA-Z0-9\\.]*)"),
    stringr::str_remove(mod$code, "\\$DATA .+([\\/a-zA-Z0-9\\.]*)")
  )
  expect_equal(nrow(out$dataset), nrow(mod$dataset))
  expect_equal(ncol(out$dataset), ncol(mod$dataset))
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})
