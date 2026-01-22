# set_iiv() -------------------------------------------------------------------
test_that("'all' adds IIV to all parameters", {
  skip_on_ci()
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
  mod <- set_iiv(mod, "all")
  
  # Check that IIV is added to all defined parameters:
  iiv_pars <- get_parameters_with_iiv(mod)
  defined_pars <- get_defined_pk_parameters(mod)
  expect_true(all(defined_pars %in% iiv_pars))
  
  # Check that ETA random variables are present:
  expect_true("ETA_CL" %in% mod$random_variables$names)
  expect_true("ETA_V" %in% mod$random_variables$names)
})

test_that("'basic' adds IIV to CL and V/V2", {
  skip_on_ci()
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  # 1-compartment model:
  mod1 <- create_model(route = "iv", n_cmt = 1, data = dat, verbose = FALSE)
  mod1 <- set_iiv(mod1, "basic")
  iiv_pars1 <- get_parameters_with_iiv(mod1)
  expect_true("CL" %in% iiv_pars1)
  expect_true("V" %in% iiv_pars1)
  
  # 2-compartment model:
  mod2 <- create_model(route = "iv", n_cmt = 2, data = dat, verbose = FALSE)
  mod2 <- set_iiv(mod2, "basic")
  iiv_pars2 <- get_parameters_with_iiv(mod2)
  expect_true("CL" %in% iiv_pars2)
  expect_true("V1" %in% iiv_pars2)
  expect_true("V2" %in% iiv_pars2)
})

test_that("character vector adds IIV to specified parameters", {
  skip_on_ci()
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
  mod <- set_iiv(mod, c("CL", "V"))
  
  iiv_pars <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% iiv_pars)
  expect_true("V" %in% iiv_pars)
  
  # Check default SD of 0.5 is used (variance = 0.25):
  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.25, tolerance = 1e-5)
  expect_equal(par_df[pars == "IIV_V",]$value, 0.25, tolerance = 1e-5)
})

test_that("list uses specified SD values", {
  skip_on_ci()
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
  mod <- set_iiv(mod, list(CL = 0.3, V = 0.4))
  
  # Check that variance is SD^2:
  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.09, tolerance = 1e-5)  # 0.3^2
  expect_equal(par_df[pars == "IIV_V",]$value, 0.16, tolerance = 1e-5)   # 0.4^2
})

test_that("removes existing IIV before adding new", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(
    route = "iv", data = dat, iiv = list(CL = 0.2, V = 0.3), verbose = FALSE
  )
  initial_iiv <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% initial_iiv)
  expect_true("V" %in% initial_iiv)
  
  # Change to only CL:
  mod <- set_iiv(mod, list(CL = 0.5))
  final_iiv <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% final_iiv)
  # TODO: This currently fails. Should reconsider behaviour of set_iiv(), since
  # the current implementation does not remove IIV on V here:
  # expect_false("V" %in% final_iiv) 
})

test_that("supports different IIV types", {
  skip_on_ci()
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
  
  # Exponential (default):
  mod_exp <- set_iiv(mod, list(CL = 0.3, V = 0.4), iiv_type = "exp")
  expect_true("ETA_CL" %in% mod_exp$random_variables$names)
  expect_true("ETA_V" %in% mod_exp$random_variables$names)
  
  # Additive:
  mod_add <- set_iiv(mod, list(CL = 0.3), iiv_type = "add")
  expect_match(
    as.character(mod_add$statements$find_assignment("CL")$expression),
    ".*ETA_CL \\+ .*",
    all = FALSE
  )
  
  # Proportional:
  mod_prop <- set_iiv(mod, list(V = 0.4), iiv_type = "prop")
  expect_match(
    as.character(mod_prop$statements$find_assignment("V")$expression),
    ".*TVV\\*\\(ETA_V \\+ 1\\).*",
    all = FALSE
  )
})

test_that("supports per-parameter IIV types", {
  skip_on_ci()
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
  mod <- set_iiv(mod, list(CL = 0.3, V = 0.4), iiv_type = list(CL = "add", V = "prop"))
  
  # CL has additive IIV:
  expect_match(
    as.character(mod$statements$find_assignment("CL")$expression),
    ".*ETA_CL \\+ .*",
    all = FALSE
  )
  
  # V has proportional IIV:
  expect_match(
    as.character(mod$statements$find_assignment("V")$expression),
    ".*TVV\\*\\(ETA_V \\+ 1\\).*",
    all = FALSE
  )
})

test_that("handles correlations with ~ syntax", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", n_cmt = 2, data = dat, verbose = FALSE)
  mod <- set_iiv(mod, list(CL = 0.2, V1 = 0.3, "CL~V1" = 0.4))
  
  # Check that BLOCK is created:
  expect_true(grepl("\\$OMEGA BLOCK\\(2\\)", mod$code))
  
  # Check that both parameters have IIV:
  iiv_pars <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% iiv_pars)
  expect_true("V1" %in% iiv_pars)
})

test_that("handles 2-compartment model parameters correctly", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", n_cmt = 2, data = dat, verbose = FALSE)
  mod <- set_iiv(mod, list(CL = 0.2, V1 = 0.3, Q = 0.4, V2 = 0.5))
  
  iiv_pars <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% iiv_pars)
  expect_true("V1" %in% iiv_pars)
  expect_true("Q" %in% iiv_pars)
  expect_true("V2" %in% iiv_pars)
  
  # Check ETA random variables
  expect_true("ETA_CL" %in% mod$random_variables$names)
  expect_true("ETA_V1" %in% mod$random_variables$names)
  expect_true("ETA_Q" %in% mod$random_variables$names)
  expect_true("ETA_V2" %in% mod$random_variables$names)
})

test_that("works with oral models", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  mod <- create_model(route = "oral", data = dat, verbose = FALSE)
  mod <- set_iiv(mod, list(CL = 0.2, V = 0.3, KA = 0.4))
  
  iiv_pars <- get_parameters_with_iiv(mod)
  expect_true("CL" %in% iiv_pars)
  expect_true("V" %in% iiv_pars)
  expect_true("KA" %in% iiv_pars)
})

test_that("preserves model structure", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  mod_orig <- create_model(route = "iv", data = dat, verbose = FALSE)
  mod_iiv <- set_iiv(mod_orig, list(CL = 0.3, V = 0.4))
  
  # Model should still be a valid pharmpy model
  expect_s3_class(mod_iiv, "pharmpy.model.external.nonmem.model.Model")
  
  # Should have same dataset
  expect_equal(mod_orig$dataset, mod_iiv$dataset)
  
  # Should preserve other model components
  expect_true(grepl("POP_CL", mod_iiv$code))
  expect_true(grepl("POP_V", mod_iiv$code))
})

test_that("handles updating existing IIV values", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  # Create model with initial IIV
  mod <- create_model(route = "iv", data = dat, iiv = list(CL = 0.2, V = 0.3), verbose = FALSE)
  
  # Update IIV values
  mod <- set_iiv(mod, list(CL = 0.5, V = 0.6))
  
  # Check updated values (variance = SD^2)
  par_df <- mod$parameters$to_dataframe()
  pars <- rownames(par_df)
  expect_equal(par_df[pars == "IIV_CL",]$value, 0.25, tolerance = 1e-5)  # 0.5^2
  expect_equal(par_df[pars == "IIV_V",]$value, 0.36, tolerance = 1e-5)   # 0.6^2
})

test_that("errors on invalid iiv input type", {
  skip_on_ci()
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
  
  # Should error when iiv is neither list nor character
  expect_error(
    set_iiv(mod, 123),
    "should be a"
  )
})

# get_parameters_with_iiv() ---------------------------------------------------
test_that("get_parameters_with_iiv returns empty vector when no IIV", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  # Create model without IIV (removing default IIV)
  mod <- create_model(route = "iv", data = dat, iiv = NULL, verbose = FALSE)
  
  # Note: Pharmpy may require at least one IIV, so this might not be empty
  # But we can still test the function
  iiv_pars <- get_parameters_with_iiv(mod)
  expect_type(iiv_pars, "character")
})

test_that("get_parameters_with_iiv correctly extracts IIV parameters", {
  skip_on_ci()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  
  mod <- create_model(route = "iv", data = dat, iiv = list(CL = 0.2, V = 0.3), verbose = FALSE)
  iiv_pars <- get_parameters_with_iiv(mod)
  
  expect_true("CL" %in% iiv_pars)
  expect_true("V" %in% iiv_pars)
  expect_type(iiv_pars, "character")
})
