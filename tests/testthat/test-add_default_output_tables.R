test_that("adding both tables when none exist works", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  # Model has no tables initially:
  expect_length(get_tables_in_model_code(mod$code), 0)
  
  # Add both tables:
  out <- add_default_output_tables(
    model = mod, tables = c("fit", "parameters"), verbose = FALSE
  )
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
  
  # Verify table content:
  expect_true(grepl("FILE=patab", out$code))
  expect_true(grepl("FILE=sdtab", out$code))
  
  # Class is preserved:
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("adding only parameters table works", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  # Model has no tables initially:
  expect_length(get_tables_in_model_code(mod$code), 0)
  
  # Add parameters table:
  out <- add_default_output_tables(
    model = mod, tables = "parameters", verbose = FALSE
  )
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  expect_false("sdtab" %in% out_tables)
  
  # Verify table content:
  expect_true(grepl("FILE=patab", out$code))
  expect_false(grepl("FILE=sdtab", out$code))
})

test_that("adding only fit table works", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  # Model has no tables initially:
  expect_length(get_tables_in_model_code(mod$code), 0)
  
  # Add fit table:
  out <- add_default_output_tables(
    model = mod, tables = "fit", verbose = FALSE
  )
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
  
  # Verify table content:
  expect_false(grepl("FILE=patab", out$code))
  expect_true(grepl("FILE=sdtab", out$code))
})

test_that("removes existing tables when remove_existing = TRUE", {
  skip_on_ci()
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
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  
  # Add a custom table:
  mod <- add_table_to_model(
    model = mod, variables = c("ID", "TIME"), firstonly = FALSE, file = "custom"
  )
  mod_tables <- get_tables_in_model_code(mod$code)
  expect_true("custom" %in% mod_tables)
  
  # Add default tables with remove_existing = TRUE
  out <- add_default_output_tables(
    model = mod,
    tables = c("fit", "parameters"),
    remove_existing = TRUE,
    verbose = FALSE
  )
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("custom" %in% out_tables)
  expect_true("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
})
