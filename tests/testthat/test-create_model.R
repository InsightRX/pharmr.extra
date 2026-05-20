test_that("create_model call without arguments works", {
  mod <- create_model(use_template = FALSE)
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
})

test_that("create_model basic functionality works", {
  # Create minimal test dataset
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test basic oral model creation
  mod_oral <- create_model(
    route = "oral",
    data = test_data,
    verbose = FALSE
  )
  expect_s3_class(mod_oral, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_MAT", mod_oral$code))
  expect_true(grepl("KA", mod_oral$code))

  # Test basic IV model creation
  mod_iv <- create_model(
    route = "iv",
    data = test_data,
    verbose = FALSE
  )
  expect_s3_class(mod_iv, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_CL", mod_iv$code))
  expect_true(!grepl("POP_MAT", mod_iv$code))
  expect_true(!grepl("POP_KA", mod_iv$code))
})

test_that("route = 'auto' infers route from CMT/EVID in data", {
  iv_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  oral_data <- iv_data
  oral_data$CMT <- c(1, 2, 2)

  expect_equal(get_route_from_data(iv_data), "iv")
  expect_equal(get_route_from_data(oral_data), "oral")
  expect_equal(get_route_from_data(NULL), "iv")
  expect_equal(get_route_from_data(iv_data[, c("ID", "TIME", "DV")]), "iv")

  mod_iv <- create_model(data = iv_data, verbose = FALSE)
  expect_s3_class(mod_iv, "pharmpy.model.external.nonmem.model.Model")
  expect_true(!grepl("POP_MAT", mod_iv$code))

  mod_oral <- create_model(data = oral_data, verbose = FALSE)
  expect_s3_class(mod_oral, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_MAT", mod_oral$code))
})

test_that("model features are correctly added", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test lag time
  mod_lag <- create_model(
    route = "oral",
    lag_time = TRUE,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("ALAG", mod_lag$code))

  # Test transit compartments
  mod_transit <- create_model(
    route = "oral",
    n_transit_compartments = 3,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("MDT", mod_transit$code))
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(TRANSIT1 DEFDOSE\\)", mod_transit$code))

  # Test multiple compartments
  mod_multi2 <- create_model(
    route = "iv",
    n_cmt = 2,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("QP1", mod_multi2$code))
  expect_true(grepl("VP1", mod_multi2$code))

  mod_multi3 <- create_model(
    route = "iv",
    n_cmt = 3,
    data = test_data,
    verbose = FALSE
  )
  expect_true(grepl("QP1", mod_multi3$code))
  expect_true(grepl("VP1", mod_multi3$code))
  expect_true(grepl("QP2", mod_multi3$code))
  expect_true(grepl("VP2", mod_multi3$code))
})

test_that("estimation methods are correctly set", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test FOCE method
  mod_foce <- create_model(
    route = "iv",
    estimation_method = "foce",
    data = test_data,
    verbose = FALSE
  )
  steps <- mod_foce$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))

  # Test SAEM method
  mod_saem <- create_model(
    route = "iv",
    estimation_method = "saem",
    data = test_data,
    verbose = FALSE
  )
  steps <- mod_saem$execution_steps$to_dataframe()
  expect_true("saem" %in% tolower(steps$method))
})

test_that("error handling works correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test invalid route
  expect_error(
    create_model(route = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid elimination
  expect_error(
    create_model(elimination = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid tool
  expect_error(
    create_model(tool = "invalid"),
    "'arg' should be one of"
  )
})

test_that("IIV settings work as expected", {
  # Test default IIV settings
  mod <- create_model()
  expect_true("ETA_CL" %in% mod$random_variables$names)
  expect_true("ETA_VC" %in% mod$random_variables$names)

  # Test custom IIV magnitudes
  mod <- create_model(iiv = list(CL = 0.4, V = 0.5))
  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.16) # 0.4^2
  expect_equal(par_df[pars == "IIV_VC",]$value, 0.25)  # 0.5^2

  # Test different IIV types
  mod <- create_model(
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = list(CL = "add", V = "prop")
  )
  expect_match(
    as.character(mod$statements$find_assignment("CL")$expression),
    ".*ETA_CL \\+ .*",
    all = FALSE
  )
  expect_match(
    as.character(mod$statements$find_assignment("VC")$expression),
    ".*POP_VC\\*\\(ETA_VC \\+ 1\\).*",
    all = FALSE
  )

  # Test no IIV (pharmpy default IIV remains since iiv=NULL skips set_iiv)
  mod <- create_model(iiv = NULL)
  expect_true("ETA_CL" %in% mod$random_variables$names)
  expect_true("ETA_VC" %in% mod$random_variables$names)
})

test_that("IIV argument works with multi-compartment models", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 2-compartment model
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_VC" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_QP1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_VP1" %in% mod_2cmt$random_variables$names)

  # Test 2-compartment model with correlation (use_template for IIV block handling)
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5, "CL~V1" = 0.4),
    data = test_data,
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_V1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_QP1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_VP1" %in% mod_2cmt$random_variables$names)

  expect_true(grepl("\\$OMEGA BLOCK\\(2\\)", mod_2cmt$code))
  expect_true(grepl("IIV_QP1", mod_2cmt$code))
  expect_true(grepl("IIV_VP1", mod_2cmt$code))

  # Test 2-compartment model with multiple correlations
  mod_2cmt2 <- create_model(
    route = "iv",
    n_cmt = 2,
    tool = "nlmixr2",
    iiv = list(
      CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5,
      "CL~V1" = 0.4, "Q~V2" = 0.3
    ),
    data = test_data,
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true(grepl("ETA_V1 \\+ ETA_QP1 \\+ ETA_VP1 \\+ ETA_CL", mod_2cmt2$code))
  expect_true(grepl("0.09,", mod_2cmt2$code))
  expect_true(grepl("0.001, 0.16,", mod_2cmt2$code))
  expect_true(grepl("0.001, 0.06, 0.25", mod_2cmt2$code))
  expect_true(grepl("0.024, 0.001, 0.001, 0.04", mod_2cmt2$code))

  ## create_model works when `parameters` table is requested
  mod_2cmt3 <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5, "CL~V2" = 0.4),
    data = test_data,
    tables = c("parameters"),
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true(grepl("\\$OMEGA BLOCK\\(2\\)", mod_2cmt3$code))
  expect_true(grepl("ID CL V1 Q V2", mod_2cmt3$code))
})

test_that("IIV argument handles edge cases correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with single parameter IIV
  mod_single <- create_model(
    route = "iv",
    iiv = list(CL = 0.2),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_single$random_variables$names)
  # VC retains default IIV from create_basic_pk_model since only CL was specified
  expect_true("ETA_VC" %in% mod_single$random_variables$names)

  # Test with very small IIV values
  mod_small <- create_model(
    route = "iv",
    iiv = list(CL = 0.01, V = 0.02),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_small$random_variables$names)
  expect_true("ETA_VC" %in% mod_small$random_variables$names)

  # Test with large IIV values
  mod_large <- create_model(
    route = "iv",
    iiv = list(CL = 1.0, V = 1.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_large$random_variables$names)
  expect_true("ETA_VC" %in% mod_large$random_variables$names)
})

test_that("IIV argument works with bioavailability parameter", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with bioavailability parameter and IIV
  mod_bio <- create_model(
    route = "oral",
    bioavailability = TRUE,
    iiv = list(CL = 0.2, V = 0.3, BIO = 0.4),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_bio$random_variables$names)
  expect_true("ETA_VC" %in% mod_bio$random_variables$names)
  expect_true("ETA_BIO" %in% mod_bio$random_variables$names)
})

test_that("IIV argument works with Michaelis-Menten elimination", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with Michaelis-Menten elimination and IIV
  mod_mm <- create_model(
    route = "iv",
    elimination = "michaelis-menten",
    iiv = list(CL = 0.2, V = 0.3, KM = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CLMM" %in% mod_mm$random_variables$names)
  expect_true("ETA_VC" %in% mod_mm$random_variables$names)
  expect_true("ETA_KM" %in% mod_mm$random_variables$names)
})

test_that("IIV argument preserves parameter initial estimates correctly", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test that IIV values are correctly converted from SD to variance
  mod <- create_model(
    route = "iv",
    iiv = list(CL = 0.3, V = 0.4),
    data = test_data,
    verbose = FALSE
  )

  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)

  # Check that IIV parameters are set to variance (SD^2)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.09)  # 0.3^2
  expect_equal(par_df[pars == "IIV_VC",]$value, 0.16)   # 0.4^2

  # Check that population parameters are preserved
  expect_true("POP_CL" %in% pars)
  expect_true("POP_VC" %in% pars)
})

test_that("IIV covariance works", {
  model_pk <- create_model(
    route = "iv",
    n_cmt = 2,
    tool = "nonmem",
    estimation_method = "foce",
    elimination = "linear",
    iiv = list(CL = 0.2, V = 0.2),
    iiv_type = "exp",
    ruv = "additive",
    uncertainty_method = "none",
    name = "run1",
    tables = c("fit"),
    use_template = TRUE,
    verbose = FALSE
  )

  model_pk2 <- set_covariance(model_pk, list("CL~V1" = 0.32))
  par_df <- model_pk2$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_CL", "IIV_V1") %in% pars))
  expect_true(stringr::str_detect(model_pk2$code, "\\$OMEGA BLOCK\\(2\\)"))

  model_pk3 <- set_iiv(model_pk, list("CL" = 0.1, "V1" = 0.1, "QP1" = 0.1))
  par_df <- model_pk3$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_QP1", "IIV_CL", "IIV_V1") %in% pars))
  expect_false(stringr::str_detect(model_pk3$code, "\\$OMEGA BLOCK\\(2\\)"))

  model_pk4 <- set_covariance(model_pk3, list("QP1~V1" = 0.32))
  par_df <- model_pk4$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_true(all(c("IIV_QP1", "IIV_CL", "IIV_V1") %in% pars))
  expect_true(stringr::str_detect(model_pk4$code, "\\$OMEGA BLOCK\\(2\\)"))

})

test_that("IIV argument works with different tools", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test with nlmixr tool
  mod_nlmixr <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    tool = "nlmixr",
    data = test_data,
    verbose = FALSE
  )
  # nlmixr models should still have the same IIV structure
  expect_true("ETA_CL" %in% mod_nlmixr$random_variables$names)
  expect_true("ETA_VC" %in% mod_nlmixr$random_variables$names)
})

test_that("RUV settings work as expected", {
  # Test proportional error
  mod <- create_model(ruv = "proportional")
  expect_equal(
    "EPS_1*IPREDADJ + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )

  # Test additive error
  mod <- create_model(ruv = "additive")
  expect_equal(
    "EPS_1 + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )

  # Test combined error
  mod <- create_model(ruv = "combined")
  expect_equal(
    "EPS_1*IPREDADJ + EPS_2 + IPRED",
    as.character(mod$statements$find_assignment("Y")$expression)
  )

  # Test log-transformed both sides
  mod <- create_model(ruv = "ltbs")
  expect_equal(
    "EPS_1 + log(IPREDADJ)",
    as.character(mod$statements$find_assignment("Y")$expression)
  )
})

test_that("LTBS model is handled, and LNDV is set to DV", {
  local_pharmr.extra_options()
  nm_data <- data.frame(
    ID = c(1, 1,1,1,1),
    AMT = c(100, 0,0,0,0),
    TIME = c(0, 1,2,3,4),
    DV = c(0, 1,2,3,4),
    LNDV = c(0, -2,-1,0,1),
    EVID = c(1, 0, 0, 0, 0),
    CMT = c(1, 1,1,1,1)
  )
  mod <- create_model(
    ruv = "ltbs",
    data = nm_data
  )
  # LNDV is mapped to NONMEM's DV variable via $INPUT (DV=LNDV).
  # The dataset columns are left untouched; the $INPUT record carries the mapping.
  expect_equal(mod$datainfo$dv_column$name, "LNDV")
  input_pairs <- mod$internals$control_stream$get_records("INPUT")[[1]]$option_pairs
  expect_equal(input_pairs[["DV"]], "LNDV")
})

test_that("mu_reference argument works correctly", {
  # auto (default): applied for SAEM, not for FOCE
  mod_saem <- create_model(estimation_method = "saem")
  expect_true(grepl("MU_1", mod_saem$code))

  mod_foce <- create_model(estimation_method = "foce")
  expect_false(grepl("MU_1", mod_foce$code))

  # TRUE: always applied regardless of estimation method
  mod_true <- create_model(estimation_method = "foce", mu_reference = TRUE)
  expect_true(grepl("MU_1", mod_true$code))

  # FALSE: never applied, even for SAEM
  mod_false <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_false(grepl("MU_1", mod_false$code))
})

test_that("IIV argument handles all input formats correctly", {
  # Test data for consistent testing
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 1: Character "all" - should add IIV to all parameters
  mod_all <- create_model(
    route = "iv",
    iiv = "all",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_all$random_variables$names)
  expect_true("ETA_VC" %in% mod_all$random_variables$names)

  # Test 2: Character "basic" - should add IIV only to CL and V
  mod_basic <- create_model(
    route = "iv",
    iiv = "basic",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_basic$random_variables$names)
  expect_true("ETA_VC" %in% mod_basic$random_variables$names)

  # Test 3: Character vector of parameter names
  mod_char_vec <- create_model(
    route = "iv",
    iiv = c("CL", "V"),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_char_vec$random_variables$names)
  expect_true("ETA_VC" %in% mod_char_vec$random_variables$names)

  # Test 4: List with numeric values (SD scale)
  mod_list <- create_model(
    route = "iv",
    iiv = list(CL = 0.3, V = 0.4),
    data = test_data,
    verbose = FALSE
  )
  par_df <- mod_list$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.09)  # 0.3^2
  expect_equal(par_df[pars == "IIV_VC",]$value, 0.16)   # 0.4^2

  # Test 5: NULL - pharmpy default IIV remains since iiv=NULL skips set_iiv
  mod_null <- create_model(
    route = "iv",
    iiv = NULL,
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_null$random_variables$names)
  expect_true("ETA_VC" %in% mod_null$random_variables$names)
})

test_that("IIV argument works with different routes", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test IV route with IIV
  mod_iv <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_iv$random_variables$names)
  expect_true("ETA_VC" %in% mod_iv$random_variables$names)

  # Test oral route with IIV (MAT parameter, not KA — pharmpy uses MAT)
  mod_oral <- create_model(
    route = "oral",
    iiv = list(CL = 0.2, V = 0.3, MAT = 0.4),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_oral$random_variables$names)
  expect_true("ETA_VC" %in% mod_oral$random_variables$names)
  expect_true("ETA_MAT" %in% mod_oral$random_variables$names)
})

test_that("IIV argument works with multi-compartment models", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test 2-compartment model
  mod_2cmt <- create_model(
    route = "iv",
    n_cmt = 2,
    iiv = list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_VC" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_QP1" %in% mod_2cmt$random_variables$names)
  expect_true("ETA_VP1" %in% mod_2cmt$random_variables$names)

  # Test 3-compartment model
  mod_3cmt <- create_model(
    route = "iv",
    n_cmt = 3,
    iiv = list(CL = 0.2, V1 = 0.3, Q2 = 0.4, V2 = 0.5, Q3 = 0.6, V3 = 0.7),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_VC" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_QP1" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_VP1" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_QP2" %in% mod_3cmt$random_variables$names)
  expect_true("ETA_VP2" %in% mod_3cmt$random_variables$names)
})

test_that("IIV argument works with different IIV types", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )

  # Test exponential IIV (default)
  mod_exp <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "exp",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_exp$random_variables$names)
  expect_true("ETA_VC" %in% mod_exp$random_variables$names)

  # Test additive IIV
  mod_add <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "add",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_add$random_variables$names)
  expect_true("ETA_VC" %in% mod_add$random_variables$names)

  # Test proportional IIV
  mod_prop <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = "prop",
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_prop$random_variables$names)
  expect_true("ETA_VC" %in% mod_prop$random_variables$names)

  # Test mixed IIV types
  mod_mixed <- create_model(
    route = "iv",
    iiv = list(CL = 0.2, V = 0.3),
    iiv_type = list(CL = "add", V = "exp"),
    data = test_data,
    verbose = FALSE
  )
  expect_true("ETA_CL" %in% mod_mixed$random_variables$names)
  expect_true("ETA_VC" %in% mod_mixed$random_variables$names)
})

test_that("create_model with scaling works", {
  local_pharmr.extra_options()
  # Create minimal test dataset
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 15, 9),  # mg/L
    AMT = c(1, 0, 0),  # 1 g
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  # Test basic oral model creation, when no IIV on V
  # Scaling requires template models for ADVAN-based compartment detection
  mod_scale1 <- create_model(
    route = "oral",
    data = test_data,
    scale_observations = 1000,
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale1$code, "S2 = V/1000"))
  expect_true(stringr::str_detect(mod_scale1$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale1$code, "\\$THETA  \\(0, 66.7\\)"))

  # Test 1-cmt oral model creation, with IIV on V
  mod_scale2 <- create_model(
    route = "oral",
    data = test_data,
    n_cmt = 1,
    iiv = list(CL = .2, V = .3),
    scale_observations = 1000,
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale2$code, "S2 = V/1000"))
  expect_true(stringr::str_detect(mod_scale2$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale2$code, "\\$THETA  \\(0, 66.7\\)"))

  # Test 2-cmt oral model creation, with IIV on V
  mod_scale3 <- create_model(
    route = "oral",
    data = test_data,
    n_cmt = 2,
    iiv = list(CL = .2, V2 = .3),
    scale_observations = 1000,
    use_template = TRUE,
    verbose = FALSE
  )
  expect_true(stringr::str_detect(mod_scale3$code, "S2 = V2/1000"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0, 34.1\\)"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0, 66.7\\)"))
  expect_true(stringr::str_detect(mod_scale3$code, "\\$THETA  \\(0,133.0\\)"))

})

test_that("create_model scaling works for nlmixr2 models", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 15, 9),
    AMT = c(1, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )

  ## Pharmpy's NONMEM->nlmixr conversion fails when S<n> scaling is combined
  ## with additive/combined error models, so scaling is injected directly on
  ## the cached nlmixr code (attr "nlmixr_code"). Verify it works for all
  ## error model types — oral (S2) and IV (S1) — including the cases that
  ## previously errored with "No resulting term found".
  for(route_arg in c("oral", "iv")) {
    sx <- if(route_arg == "oral") "S2" else "S1"
    for(err in c("proportional", "additive", "combined")) {
      mod <- create_model(
        route = route_arg,
        data = test_data,
        ruv = err,
        scale_observations = 1000,
        tool = "nlmixr2",
        verbose = FALSE
      )
      code <- attr(mod, "nlmixr_code")
      expect_true(
        stringr::str_detect(code, paste0(sx, " <- VC/1000")),
        info = paste("route=", route_arg, "err=", err)
      )
      expect_true(
        stringr::str_detect(code, paste0("IPRED <- A_CENTRAL/", sx)),
        info = paste("route=", route_arg, "err=", err)
      )
      ## Initial estimates scaled by the same factor as the NONMEM path
      expect_true(
        stringr::str_detect(code, "POP_CL <- c\\(0\\.0, 34\\.1, Inf\\)"),
        info = paste("route=", route_arg, "err=", err)
      )
      expect_true(
        stringr::str_detect(code, "POP_VC <- c\\(0\\.0, 66\\.7, Inf\\)"),
        info = paste("route=", route_arg, "err=", err)
      )
    }
  }
})

test_that("create_model BLQ with LLOQ coded in DV works", {

  ## This behavior was deprecated. Any dataset passed to
  ## `create_model()` is now used unchanged.
  
  # # Create minimal test dataset
  # test_data <- data.frame(
  #   ID = 1,
  #   TIME = c(0, 1, 2),
  #   DV = c(0, 10, "<3"),
  #   AMT = c(100, 0, 0),
  #   CMT = 1,
  #   EVID = c(1, 0, 0),
  #   MDV = c(1, 0, 0),
  #   BW = 70
  # )

  # # Test basic oral model creation
  # mod_oral <- create_model(
  #   route = "oral",
  #   data = test_data,
  #   verbose = FALSE
  # )
  # expect_s3_class(mod_oral, "pharmpy.model.external.nonmem.model.Model")
  # expect_equal(mod_oral$dataset$LLOQ, c(0, 0, 3))
})
  
## TMDD models
test_that("create_model with TMDD but unknown tmdd_type fails", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  expect_error({
    mod_sc_tmdd <- create_model(
      route = "oral",
      data = test_data,
      tmdd_type = "bla",
      verbose = FALSE
    )
  })
})

# -- dictionary + drop_input + original dataset preservation tests ------------

test_that("dictionary renames columns in $INPUT", {
  local_pharmr.extra_options()
  dat <- data.frame(
    SUBJID = 1,
    TAFD = c(0, 1, 2),
    CONC = c(0, 10, 5),
    AMT = c(100, 0, 0),
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    CMT = 1
  )
  mod <- create_model(
    route = "iv", data = dat, verbose = FALSE,
    dictionary = list(ID = "SUBJID", TIME = "TAFD", DV = "CONC")
  )
  obj <- nm_read_model(code = mod$code)
  input_line <- paste(obj[["INPUT"]], collapse = " ")
  expect_true(grepl("\\bID\\b", input_line))
  expect_true(grepl("\\bTIME\\b", input_line))
  expect_true(grepl("\\bDV\\b", input_line))
  expect_false(grepl("\\bSUBJID\\b", input_line))
  expect_false(grepl("\\bTAFD\\b", input_line))
  expect_false(grepl("\\bCONC\\b", input_line))
})

test_that("dictionary drops conflicting column when standard name already exists", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    TAFD = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    CMT = 1
  )
  mod <- create_model(
    route = "iv", data = dat, verbose = FALSE,
    dictionary = list(TIME = "TAFD")
  )
  obj <- nm_read_model(code = mod$code)
  input_line <- paste(obj[["INPUT"]], collapse = " ")
  # Original TIME position should become a placeholder (replaced with DROP in run folder)
  expect_true(grepl("_DDRP_TIME", input_line))
  # TAFD position should now be TIME
  expect_true(grepl("\\bTIME\\b", input_line))
})

test_that("original dataset is preserved in run folder with dictionary and drop_input", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TAFD = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    CMT = 1,
    BW = c(70, 70, 70)
  )
  mod <- create_model(
    route = "iv", data = dat, verbose = FALSE,
    dictionary = list(TIME = "TAFD"),
    drop_input = c("BW")
  )

  # Simulate what run_nlme does: save/restore attr across set_name
  original_data <- attr(mod, "original_data")
  mod <- pharmr::set_name(mod, new_name = "test_run")
  attr(mod, "original_data") <- original_data

  run_dir <- tempfile("run_")
  dir.create(run_dir)
  obj <- prepare_run_folder(
    id = basename(run_dir), model = mod, path = dirname(run_dir),
    force = TRUE, verbose = FALSE
  )

  written <- read.csv(obj$dataset_path, check.names = FALSE, nrows = 3)

  # CSV must be identical to original input data
  expect_equal(names(written), names(dat))
  expect_equal(ncol(written), ncol(dat))
  # Original column name preserved (not renamed to TIME)
  expect_true("TAFD" %in% names(written))
  expect_false("TIME" %in% names(written))

  # $INPUT in model file uses standard names and DROP
  mod_code <- readLines(file.path(obj$fit_folder, obj$model_file)) |> paste(collapse = "\n")
  mod_obj <- nm_read_model(code = mod_code)
  input_line <- paste(mod_obj[["INPUT"]], collapse = " ")
  expect_true(grepl("\\bTIME\\b", input_line))
  expect_true(grepl("\\bDROP\\b", input_line))

  unlink(run_dir, recursive = TRUE)
})

test_that("create_model basic TDMDD model (full) from 1-cmt sc model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_sc_tmdd <- create_model(
    route = "oral",
    data = test_data,
    tmdd_type = "full",
    verbose = FALSE
  )
  expect_s3_class(mod_sc_tmdd, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("KA", mod_sc_tmdd$code))
  expect_true(grepl("POP_R_0", mod_sc_tmdd$code))
  expect_true("POP_R_0" %in% mod_sc_tmdd$parameters$names)
})

test_that("create_model basic TDMDD model (full) from 1-cmt iv model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_iv_tmdd <- create_model(
    route = "iv",
    data = test_data,
    tmdd_type = "full",
    verbose = FALSE
  )
  expect_s3_class(mod_iv_tmdd, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("POP_KA", mod_iv_tmdd$code))
  expect_true(grepl("POP_R_0", mod_iv_tmdd$code))
  expect_true("POP_R_0" %in% mod_iv_tmdd$parameters$names)
  expect_true("POP_KON" %in% mod_iv_tmdd$parameters$names)
})

test_that("create_model basic TDMDD model (CR) from 1-cmt iv model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_iv_tmdd_cr <- create_model(
    route = "iv",
    data = test_data,
    tmdd_type = "cr",
    verbose = FALSE
  )
  expect_s3_class(mod_iv_tmdd_cr, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("POP_KA", mod_iv_tmdd_cr$code))
  expect_true(grepl("POP_R_0", mod_iv_tmdd_cr$code))
  expect_true("POP_R_0" %in% mod_iv_tmdd_cr$parameters$names)
  expect_true("POP_KON" %in% mod_iv_tmdd_cr$parameters$names)
})

test_that("create_model basic TDMDD model (ib) from 1-cmt iv model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_iv_tmdd_ib <- create_model(
    route = "iv",
    data = test_data,
    tmdd_type = "ib",
    verbose = FALSE
  )
  expect_s3_class(mod_iv_tmdd_ib, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("POP_KA", mod_iv_tmdd_ib$code))
  expect_true(grepl("POP_R_0", mod_iv_tmdd_ib$code))
  expect_true("POP_R_0" %in% mod_iv_tmdd_ib$parameters$names)
  expect_true("POP_KON" %in% mod_iv_tmdd_ib$parameters$names)
})

test_that("create_model basic TDMDD model (cr+ib) from 1-cmt iv model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_iv_tmdd_crib <- create_model(
    route = "iv",
    data = test_data,
    tmdd_type = "crib",
    verbose = FALSE
  )
  expect_s3_class(mod_iv_tmdd_crib, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("POP_KA", mod_iv_tmdd_crib$code))
  expect_true(grepl("POP_R_0", mod_iv_tmdd_crib$code))
  expect_true("POP_R_0" %in% mod_iv_tmdd_crib$parameters$names)
  expect_true("POP_KON" %in% mod_iv_tmdd_crib$parameters$names)
})

test_that("create_model basic TDMDD model (QSS) from 1-cmt iv model works", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_iv_tmdd_qss <- create_model(
    route = "iv",
    data = test_data,
    tmdd_type = "qss",
    verbose = FALSE
  )
  expect_s3_class(mod_iv_tmdd_qss, "pharmpy.model.external.nonmem.model.Model")
  expect_false(grepl("POP_KA", mod_iv_tmdd_qss$code))
  expect_true(grepl("POP_R_0", mod_iv_tmdd_qss$code))
  expect_true("POP_R_0" %in% mod_iv_tmdd_qss$parameters$names)
  expect_false("POP_KON" %in% mod_iv_tmdd_qss$parameters$names)
})

## Metabolite models
test_that("create_model can create metabolite model from iv", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    DVID = c(0, 1, 2),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_metab <- create_model(
    route = "iv", 
    n_cmt = 1,
    data = test_data,
    metabolite = TRUE,
    verbose = FALSE
  )
  expect_s3_class(mod_metab, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(CENTRAL DEFDOSE\\) COMPARTMENT=\\(METABOLITE\\)", mod_metab$code))
  expect_true(grepl("CLM = ", mod_metab$code))
  expect_true(grepl("VM = ", mod_metab$code))
  expect_true("POP_CLM" %in% mod_metab$parameters$names)
  expect_true("POP_VM" %in% mod_metab$parameters$names)
})

test_that("create_model can create metabolite model from oral", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    DVID = c(0, 1, 2),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_metab_oral <- create_model(
    route = "oral", 
    n_cmt = 1,
    data = test_data,
    metabolite = TRUE,
    verbose = FALSE
  )
  expect_s3_class(mod_metab_oral, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(DEPOT DEFDOSE\\) COMPARTMENT=\\(CENTRAL\\) COMPARTMENT=\\(METABOLITE\\)", mod_metab_oral$code))
  expect_true(grepl("CLM = ", mod_metab_oral$code))
  expect_true(grepl("VM = ", mod_metab_oral$code))
  expect_true("POP_CLM" %in% mod_metab_oral$parameters$names)
  expect_true("POP_VM" %in% mod_metab_oral$parameters$names)
})

test_that("create_model can create metabolite model with explicit arguments", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    DVID = c(0, 1, 2),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod_metab_oral2 <- create_model(
    route = "oral", 
    n_cmt = 1,
    data = test_data,
    metabolite = list(drug_dvid = 1, presystemic = TRUE),
    verbose = FALSE
  )
  expect_s3_class(mod_metab_oral2, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(DEPOT DEFDOSE\\) COMPARTMENT=\\(CENTRAL\\) COMPARTMENT=\\(METABOLITE\\)", mod_metab_oral2$code))
  expect_true(grepl("K12 = \\(1 - FPRE\\)/MAT", mod_metab_oral2$code))

  ## now with presystemic set to FALSE
  mod_metab_oral3 <- create_model(
    route = "oral",
    n_cmt = 1,
    data = test_data,
    metabolite = list(drug_dvid = 1, presystemic = FALSE),
    verbose = FALSE
  )
  expect_s3_class(mod_metab_oral3, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("\\$MODEL COMPARTMENT=\\(DEPOT DEFDOSE\\) COMPARTMENT=\\(CENTRAL\\) COMPARTMENT=\\(METABOLITE\\)", mod_metab_oral3$code))
  expect_false(grepl("K12 = \\(1 - FPRE\\)/MAT", mod_metab_oral3$code))

  ## missing arguments
  expect_error(
    create_model(
      route = "oral", 
      n_cmt = 1,
      data = test_data,
      metabolite = list(drug_dvid = 1, bla = FALSE),
      verbose = FALSE
    ),
    "When `metabolite` is specified"
  )
  ## mismatched arguments
  expect_error(
    create_model(
      route = "iv", 
      n_cmt = 1,
      data = test_data,
      metabolite = list(drug_dvid = 1, presystemic = TRUE),
      verbose = FALSE
    ),
    "Cannot add presystemic metabolite"
  )
})

test_that("create_model supports multiple estimation methods", {
  local_pharmr.extra_options()
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = 70
  )
  mod <- create_model(
    route = "iv",
    data = test_data,
    estimation_method = c("saem", "imp"),
    verbose = FALSE
  )
  steps <- mod$execution_steps$to_dataframe()
  expect_equal(nrow(steps), 2)
  expect_equal(tolower(steps$method[1]), "saem")
  expect_equal(tolower(steps$method[2]), "imp")
})

test_that("create_model errors on invalid estimation method", {
  local_pharmr.extra_options()
  expect_error(
    create_model(route = "iv", data = test_data, estimation_method = "BOGUS", verbose = FALSE),
    "estimation_method must be"
  )
})

## get_template_modelfile tests (no pharmpy required) ----

test_that("get_template_modelfile returns analytical templates when force_ode = FALSE", {
  path_iv   <- pharmr.extra:::get_template_modelfile("iv",   n_cmt = 1, force_ode = FALSE)
  path_oral <- pharmr.extra:::get_template_modelfile("oral", n_cmt = 1, force_ode = FALSE)
  expect_match(path_iv,   "base_iv\\.mod$")
  expect_match(path_oral, "base_oral\\.mod$")
  expect_false(grepl("_ode", path_iv))
  expect_false(grepl("_ode", path_oral))
})

test_that("get_template_modelfile returns ODE template for 1-cmt when force_ode = TRUE", {
  path_iv   <- pharmr.extra:::get_template_modelfile("iv",   n_cmt = 1, force_ode = TRUE)
  path_oral <- pharmr.extra:::get_template_modelfile("oral", n_cmt = 1, force_ode = TRUE)
  expect_match(path_iv,   "base_iv_ode\\.mod$")
  expect_match(path_oral, "base_oral_ode\\.mod$")
})

test_that("get_template_modelfile returns multi-cmt ODE templates when force_ode = TRUE", {
  path_2cmt_iv   <- pharmr.extra:::get_template_modelfile("iv",   n_cmt = 2, force_ode = TRUE)
  path_2cmt_oral <- pharmr.extra:::get_template_modelfile("oral", n_cmt = 2, force_ode = TRUE)
  path_3cmt_iv   <- pharmr.extra:::get_template_modelfile("iv",   n_cmt = 3, force_ode = TRUE)
  path_3cmt_oral <- pharmr.extra:::get_template_modelfile("oral", n_cmt = 3, force_ode = TRUE)
  expect_match(path_2cmt_iv,   "2cmt_iv_ode\\.mod$")
  expect_match(path_2cmt_oral, "2cmt_oral_ode\\.mod$")
  expect_match(path_3cmt_iv,   "3cmt_iv_ode\\.mod$")
  expect_match(path_3cmt_oral, "3cmt_oral_ode\\.mod$")
})

test_that("get_template_modelfile returns ODE template for valid ADVAN numbers", {
  for (advan in c(6, 9, 13)) {
    path <- pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = advan)
    expect_match(path, "_ode\\.mod$", info = paste("ADVAN", advan))
    # file must actually exist
    expect_true(file.exists(path), info = paste("ADVAN", advan))
  }
})

test_that("get_template_modelfile returns character ADVAN numbers as valid input", {
  path <- pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = "6")
  expect_match(path, "_ode\\.mod$")
})

test_that("get_template_modelfile errors on invalid ADVAN numbers", {
  expect_error(
    pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = 5),
    "force_ode.*can only be"
  )
  expect_error(
    pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = 7),
    "force_ode.*can only be"
  )
})

test_that("get_template_modelfile errors on non-numeric string for force_ode", {
  expect_error(
    pharmr.extra:::get_template_modelfile("iv", n_cmt = 1, force_ode = "foo"),
    "force_ode.*must be"
  )
})

test_that("get_template_modelfile returns existing files", {
  combos <- expand.grid(
    route     = c("iv", "oral"),
    n_cmt     = c(1, 2, 3),
    force_ode = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(combos))) {
    path <- pharmr.extra:::get_template_modelfile(
      combos$route[i], combos$n_cmt[i], combos$force_ode[i]
    )
    expect_true(
      file.exists(path),
      info = paste("route:", combos$route[i], "n_cmt:", combos$n_cmt[i],
                   "force_ode:", combos$force_ode[i])
    )
  }
})

## create_model force_ode tests ----

test_that("create_model default (force_ode = FALSE) creates analytical model", {
  mod_iv   <- create_model(route = "iv",   n_cmt = 1, verbose = FALSE)
  mod_oral <- create_model(route = "oral", n_cmt = 1, verbose = FALSE)
  expect_false(grepl("ADVAN6", mod_iv$code))
  expect_false(grepl("\\$DES",  mod_iv$code))
  expect_false(grepl("ADVAN6", mod_oral$code))
  expect_false(grepl("\\$DES",  mod_oral$code))
})

test_that("create_model force_ode = TRUE creates ODE-based model", {
  mod_iv   <- create_model(route = "iv",   n_cmt = 1, force_ode = TRUE, use_template = TRUE, verbose = FALSE)
  mod_oral <- create_model(route = "oral", n_cmt = 1, force_ode = TRUE, use_template = TRUE, verbose = FALSE)
  expect_true(grepl("ADVAN6", mod_iv$code))
  expect_true(grepl("\\$DES",  mod_iv$code))
  expect_true(grepl("ADVAN6", mod_oral$code))
  expect_true(grepl("\\$DES",  mod_oral$code))
})

test_that("create_model force_ode = 6 is equivalent to force_ode = TRUE", {
  mod_true <- create_model(route = "iv", n_cmt = 1, force_ode = TRUE, use_template = TRUE, verbose = FALSE)
  mod_6    <- create_model(route = "iv", n_cmt = 1, force_ode = 6,    use_template = TRUE, verbose = FALSE)
  expect_true(grepl("ADVAN6", mod_6$code))
  expect_true(grepl("\\$DES",  mod_6$code))
  expect_equal(mod_true$code, mod_6$code)
})

test_that("create_model force_ode = TRUE with n_cmt = 2 uses 2-cmt ODE template", {
  mod <- create_model(route = "iv", n_cmt = 2, force_ode = TRUE, use_template = TRUE, verbose = FALSE)
  expect_true(grepl("ADVAN6", mod$code))
  expect_true(grepl("\\$DES",  mod$code))
  # 2-cmt ODE has equations for two compartments
  expect_true(grepl("DADT\\(2\\)", mod$code))
})

test_that("create_model force_ode = TRUE with n_cmt = 3 uses 3-cmt ODE template", {
  mod <- create_model(route = "iv", n_cmt = 3, force_ode = TRUE, use_template = TRUE, verbose = FALSE)
  expect_true(grepl("ADVAN6", mod$code))
  expect_true(grepl("\\$DES",  mod$code))
  expect_true(grepl("DADT\\(3\\)", mod$code))
})

test_that("create_model force_ode with invalid ADVAN number errors", {
  expect_error(
    create_model(route = "iv", force_ode = 5, use_template = TRUE, verbose = FALSE),
    "force_ode.*can only be"
  )
})


test_that("create_model accepts data as CSV filename", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv(test_data, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  mod <- create_model(route = "iv", data = tmp, auto_stack_encounters = FALSE, verbose = FALSE)
  expect_s3_class(mod, "pharmpy.model.external.nonmem.model.Model")
  expect_true(grepl("POP_CL", mod$code))
})


test_that("create_model with filename data warns when auto_stack_encounters is TRUE", {
  test_data <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv(test_data, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  expect_warning(
    create_model(route = "iv", data = tmp, auto_stack_encounters = TRUE, verbose = FALSE),
    "auto_stack_encounters.*can only be used when.*data.frame"
  )
})
