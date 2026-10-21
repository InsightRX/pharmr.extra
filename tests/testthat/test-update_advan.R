ode_template <- function() {
  pharmr::read_model(
    pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = TRUE)
  )
}

test_that("update_advan switches ADVAN6 to ADVAN13 and replaces TRANS with TOL", {
  mod <- ode_template()
  expect_match(mod$code, "ADVAN6")
  new_mod <- update_advan(mod, 13)
  expect_true(inherits(new_mod, "pharmpy.model.model.Model"))
  expect_match(new_mod$code, "ADVAN13")
  expect_no_match(new_mod$code, "ADVAN6")
  expect_no_match(new_mod$code, "TRANS[0-9]")
  expect_match(new_mod$code, "TOL=9")
  expect_equal(get_advan(new_mod), 13L)
})

test_that("update_advan supports ADVAN9", {
  new_mod <- update_advan(ode_template(), 9)
  expect_equal(get_advan(new_mod), 9L)
})

test_that("update_advan keeps the model's dataset", {
  mod <- ode_template()
  data <- data.frame(
    ID = c(1, 1), TIME = c(0, 1), DV = c(0, 10), MDV = c(1, 0),
    EVID = c(1, 0), SS = 0, II = 0, AMT = c(100, 0)
  )
  mod <- pharmr::set_dataset(mod, data)
  new_mod <- update_advan(mod, 13)
  expect_false(is.null(new_mod$dataset))
  expect_equal(nrow(new_mod$dataset), 2)
})

test_that("update_advan rejects unsupported ADVANs", {
  expect_error(update_advan(ode_template(), 2), "Only supported for ADVAN 6, 9, or 13")
})
