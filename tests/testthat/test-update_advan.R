ode_model_code <- paste0(
  "$PROBLEM Test\n",
  "$INPUT ID TIME DV AMT EVID MDV\n",
  "$DATA data.csv IGNORE=@\n",
  "$SUBROUTINES ADVAN6 TOL=9\n",
  "$MODEL COMP=(CENT DEFDOSE)\n",
  "$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
  "$DES\nDADT(1)=-CL/V*A(1)\n",
  "$ERROR\nY=F+EPS(1)\n",
  "$THETA (0,10)\n$THETA (0,50)\n$SIGMA 0.1\n$EST METHOD=1\n"
)

test_that("update_advan only accepts ODE ADVANs", {
  mod <- pharmr::read_model_from_string(ode_model_code)
  expect_error(update_advan(mod, 1), "Only supported for ADVAN 6, 9, or 13")
})

test_that("update_advan switches the ADVAN number and leaves the rest alone", {
  mod <- pharmr::read_model_from_string(ode_model_code)
  res <- update_advan(mod, 13)
  expect_match(res$code, "\\$SUBROUTINES ADVAN13 TOL=9")
  expect_false(grepl("ADVAN6", res$code))
  expect_match(res$code, "DADT\\(1\\)=-CL/V\\*A\\(1\\)")
  expect_null(res$dataset)
})

test_that("update_advan replaces TRANS with TOL", {
  code <- sub("ADVAN6 TOL=9", "ADVAN6 TRANS1", ode_model_code, fixed = TRUE)
  mod <- pharmr::read_model_from_string(code)
  res <- update_advan(mod, 9)
  expect_match(res$code, "\\$SUBROUTINES ADVAN9 TOL=9")
  expect_false(grepl("TRANS", res$code))
})

test_that("update_advan keeps the model's dataset", {
  mod <- pharmr::read_model_from_string(ode_model_code)
  df <- data.frame(
    ID = c(1, 1), TIME = c(0, 1), DV = c(0, 1),
    AMT = c(100, 0), EVID = c(1, 0), MDV = c(1, 0)
  )
  mod <- pharmr::set_dataset(mod, df, datatype = "nonmem")
  res <- update_advan(mod, 13)
  expect_equal(nrow(res$dataset), 2)
  expect_equal(as.numeric(res$dataset$DV), c(0, 1))
})

test_that("update_advan works on the built-in ODE template used by create_model()", {
  mod <- pharmr::read_model(
    pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = TRUE)
  )
  res <- update_advan(mod, 13)
  expect_equal(get_advan(res), 13L)
  expect_false(grepl("TRANS[0-9]", res$code))
})
