test_that("works when no patab exists initially", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, use_template = TRUE, verbose = FALSE)

  # Model has no tables initially:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_length(existing_tables, 0)
  
  # Update PK tables:
  out <- update_pk_tables(model = mod)
  
  # Verify patab was added:
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  expect_true(grepl("FILE=patab", out$code))
})

test_that("removes existing patab and adds it back", {
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
  
  # Model has patab initially:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_true("patab" %in% existing_tables)
  
  # Update PK tables:
  out <- update_pk_tables(model = mod)
  
  # Verify patab still exists (was removed and re-added):
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  expect_true(grepl("FILE=patab", out$code))
  
  # Class is preserved:
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("removes multiple patab tables", {
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
  
  # Add a custom patab table:
  mod <- add_table_to_model(
    model = mod,
    variables = c("ID", "TIME"),
    firstonly = FALSE,
    file = "patab_custom"
  )
  
  # Verify both patab tables exist:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_true("patab" %in% existing_tables)
  expect_true("patab_custom" %in% existing_tables)
  
  # Update PK tables:
  out <- update_pk_tables(model = mod)
  
  # Verify both patab tables were removed and only one patab was added back:
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("patab_custom" %in% out_tables)
  expect_true("patab" %in% out_tables)
  
  # Count occurrences of FILE=patab in code (should be exactly 1):
  patab_matches <- stringr::str_count(out$code, "FILE=patab")
  expect_equal(patab_matches, 1)
})

test_that("preserves other tables like sdtab", {
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
  
  # Model has both tables initially:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_true("patab" %in% existing_tables)
  expect_true("sdtab" %in% existing_tables)
  
  # Verify sdtab is preserved:
  out <- update_pk_tables(model = mod)
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
  expect_true(grepl("FILE=sdtab", out$code))
})

test_that("preserves the dataset", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0),
    BW = c(70, 70, 70)
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  out <- update_pk_tables(model = mod)
  
  expect_equal(out$dataset, mod$dataset, ignore_attr = TRUE)
})

test_that("works with oral model", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3),
    DV = c(0, 10, 5, 3),
    AMT = c(100, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 0, 0)
  )
  mod <- create_model(
    route = "oral", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  
  # Update PK tables:
  out <- update_pk_tables(model = mod)
  
  # Verify patab exists:
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  
  # Verify model still has oral-specific parameters (KA):
  expect_true(grepl("KA", out$code) || grepl("POP_KA", out$code))
})

test_that("works with 2-compartment model", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3, 4),
    DV = c(0, 10, 5, 3, 2),
    AMT = c(100, 0, 0, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 0, 0),
    MDV = c(1, 0, 0, 0, 0)
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  
  # Add peripheral compartment:
  mod <- pharmr::add_peripheral_compartment(mod)
  
  # Update PK tables:
  out <- update_pk_tables(model = mod)
  
  # Verify patab exists:
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("patab" %in% out_tables)
  
  # Verify 2-compartment parameters are present:
  expect_true(grepl("Q", out$code) || grepl("QP1", out$code))
  expect_true(grepl("V2", out$code) || grepl("VP1", out$code))
})
