test_that("get_declared_variables returns user-declared variables", {
  mod <- pharmr::read_model_from_string(paste0(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n",
    "$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
    "$ERROR\nIPRED=F\nW=1\nY=IPRED+W*EPS(1)\n",
    "$THETA (0,10)\n$THETA (0,50)\n$SIGMA 0.1\n$EST METHOD=1\n"
  ))
  vars <- get_declared_variables(mod)
  expect_type(vars, "character")
  expect_setequal(vars, c("CL", "V", "S1", "IPRED"))
  ## NONMEM-internal variables are dropped
  expect_false(any(c("Y", "F", "W") %in% vars))
})

test_that("get_declared_variables drops subscripted variables", {
  mod <- pharmr::read_model_from_string(paste0(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n",
    "$SUBROUTINES ADVAN6 TOL=9\n$MODEL COMP=(CENT DEFDOSE)\n",
    "$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n",
    "$DES\nDADT(1)=-CL/V*A(1)\n$ERROR\nY=F+EPS(1)\n",
    "$THETA (0,10)\n$THETA (0,50)\n$SIGMA 0.1\n$EST METHOD=1\n"
  ))
  vars <- get_declared_variables(mod)
  expect_false(any(grepl("\\(", vars)))
  expect_true(all(c("CL", "V", "S1") %in% vars))
})
