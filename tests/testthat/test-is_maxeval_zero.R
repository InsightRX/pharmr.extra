maxeval_model <- function(est) {
  pharmr::read_model_from_string(paste0(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n",
    "$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
    "$ERROR\nY=F+EPS(1)\n$THETA (0,10)\n$THETA (0,50)\n$SIGMA 0.1\n",
    est, "\n"
  ))
}

test_that("is_maxeval_zero detects MAXEVAL=0", {
  expect_true(is_maxeval_zero(maxeval_model("$EST METHOD=1 MAXEVAL=0")))
})

test_that("is_maxeval_zero is FALSE when the model will be estimated", {
  expect_false(is_maxeval_zero(maxeval_model("$EST METHOD=1 MAXEVAL=9999")))
  expect_false(is_maxeval_zero(maxeval_model("$EST METHOD=SAEM")))
})

test_that("is_maxeval_zero only looks at the last estimation step", {
  mod <- maxeval_model("$EST METHOD=1 MAXEVAL=9999\n$EST METHOD=1 MAXEVAL=0")
  expect_true(is_maxeval_zero(mod))
  mod <- maxeval_model("$EST METHOD=1 MAXEVAL=0\n$EST METHOD=1 MAXEVAL=9999")
  expect_false(is_maxeval_zero(mod))
})
