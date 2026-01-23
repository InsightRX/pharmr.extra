test_that("basic search space creation works with default parameters", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE")
  )
  
  expect_type(out, "character")
  expect_equal(out, "COVARIATE?([CL,V], [WT,AGE], [LIN,POW])")
})

test_that("explore = FALSE removes question mark", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    explore = FALSE
  )
  
  expect_equal(out, "COVARIATE([CL,V], [WT,AGE], [LIN,POW])")
})

test_that("single operation works", {
  out <- create_covariate_search_space(
    parameters = c("CL"),
    covariates = c("WT"),
    operation = "LIN"
  )
  
  expect_equal(out, "COVARIATE?([CL], [WT], [LIN])")
})

test_that("multiple operations work", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    operation = c("LIN", "POW", "EXP")
  )
  
  expect_equal(out, "COVARIATE?([CL,V], [WT,AGE], [LIN,POW,EXP])")
})

test_that("operation '*' is handled correctly", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    operation = "*"
  )
  
  expect_equal(out, "COVARIATE?([CL,V], [WT,AGE], *)")
})

test_that("single parameter works", {
  out <- create_covariate_search_space(
    parameters = "CL",
    covariates = c("WT", "AGE", "SEX")
  )
  
  expect_equal(out, "COVARIATE?([CL], [WT,AGE,SEX], [LIN,POW])")
})

test_that("single covariate works", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V", "KA"),
    covariates = "WT"
  )
  
  expect_equal(out, "COVARIATE?([CL,V,KA], [WT], [LIN,POW])")
})

test_that("structural model parameters are included when specified", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    struct_parameters = c("KA"),
    struct_covariates = c("SEX"),
    struct_operation = "LIN"
  )
  
  expect_equal(
    out,
    "COVARIATE([KA], [SEX], [LIN]); COVARIATE?([CL,V], [WT,AGE], [LIN,POW])"
  )
})

test_that("structural model with multiple operations works", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    struct_parameters = c("KA"),
    struct_covariates = c("SEX", "RACE"),
    struct_operation = c("LIN", "POW")
  )
  
  expect_equal(
    out,
    "COVARIATE([KA], [SEX,RACE], [LIN,POW]); COVARIATE?([CL,V], [WT,AGE], [LIN,POW])"
  )
})

test_that("structural model with '*' operation works", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    struct_parameters = c("KA"),
    struct_covariates = c("SEX"),
    struct_operation = "*"
  )
  
  expect_equal(
    out,
    "COVARIATE([KA], [SEX], *); COVARIATE?([CL,V], [WT,AGE], [LIN,POW])"
  )
})

test_that("error when struct_parameters provided without struct_covariates", {
  expect_error(
    create_covariate_search_space(
      parameters = c("CL", "V"),
      covariates = c("WT", "AGE"),
      struct_parameters = c("KA")
    ),
    "Please also specify structural covariates to include"
  )
})

test_that("no structural model when struct_parameters is NULL", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    struct_parameters = NULL
  )
  
  expect_equal(out, "COVARIATE?([CL,V], [WT,AGE], [LIN,POW])")
})

test_that("complex example with all features works", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V", "KA"),
    covariates = c("WT", "AGE", "SEX", "RACE"),
    operation = c("LIN", "POW", "EXP"),
    explore = TRUE,
    struct_parameters = c("V2", "Q"),
    struct_covariates = c("WT"),
    struct_operation = "POW"
  )
  
  expect_equal(
    out,
    "COVARIATE([V2,Q], [WT], [POW]); COVARIATE?([CL,V,KA], [WT,AGE,SEX,RACE], [LIN,POW,EXP])"
  )
})

test_that("explore = FALSE with structural model works", {
  out <- create_covariate_search_space(
    parameters = c("CL", "V"),
    covariates = c("WT", "AGE"),
    explore = FALSE,
    struct_parameters = c("KA"),
    struct_covariates = c("SEX"),
    struct_operation = "LIN"
  )
  
  expect_equal(
    out,
    "COVARIATE([KA], [SEX], [LIN]); COVARIATE([CL,V], [WT,AGE], [LIN,POW])"
  )
})
