test_that("get_covariate_data returns one row per subject with the model's covariates", {
  mod <- pharmr::read_model_from_string(paste0(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV WT\n$DATA data.csv IGNORE=@\n",
    "$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)*EXP(ETA(1))*(WT/70)\nV=THETA(2)\nS1=V\n",
    "$ERROR\nY=F+EPS(1)\n$THETA (0,10)\n$THETA (0,50)\n$OMEGA 0.1\n$SIGMA 0.1\n",
    "$EST METHOD=1\n"
  ))
  df <- data.frame(
    ID = rep(1:2, each = 2), TIME = c(0, 1, 0, 1), DV = c(0, 1, 0, 2),
    AMT = c(100, 0, 100, 0), EVID = c(1, 0, 1, 0), MDV = c(1, 0, 1, 0),
    WT = c(70, 70, 80, 80)
  )
  mod <- pharmr::set_dataset(mod, df, datatype = "nonmem")
  res <- get_covariate_data(mod)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("ID", "WT"))
  expect_equal(as.numeric(res$ID), c(1, 2))
  expect_equal(as.numeric(res$WT), c(70, 80))
})
