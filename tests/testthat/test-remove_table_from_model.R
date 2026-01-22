test_that("adds table correctly to model with no existing tables", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  # Model has no tables initially:
  expect_length(get_tables_in_model_code(mod$code), 0)
  
  # Add a table:
  out <- remove_table_from_model(
    model = mod,
    variables = c("ID", "CL", "V"),
    firstonly = FALSE,
    file = "mytab"
  )
  
  # Verify table was added:
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("mytab" %in% out_tables)
  
  # Verify table content:
  expect_true(grepl("FILE=mytab", out$code))
  expect_true(grepl("ID CL V", out$code))
  expect_true(grepl("NOAPPEND NOPRINT", out$code))
  expect_false(grepl("FIRSTONLY", out$code))
  
  # Class is preserved:
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("adds table with FIRSTONLY when firstonly = TRUE", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  out <- remove_table_from_model(
    model = mod,
    variables = c("ID", "TIME", "DV"),
    firstonly = TRUE,
    file = "mytab"
  )
  
  out_tables <- get_tables_in_model_code(out$code)
  expect_true("mytab" %in% out_tables)
  expect_true(grepl("FIRSTONLY", out$code))
})

test_that("warns when file already exists in model", {
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
  mod <- create_model(route = "iv", data = dat, tables = c("fit"), verbose = FALSE)
  # Model already has sdtab:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_true("sdtab" %in% existing_tables)
  
  # Try to add a table with the same file name:
  expect_warning(
    out <- remove_table_from_model(
      model = mod,
      variables = c("ID", "CL"),
      firstonly = FALSE,
      file = "sdtab"
    ),
    "Table file already in a \\$TABLE record in model"
  )
  
  # Model should be returned unchanged:
  expect_equal(out$code, mod$code)
})

test_that("warns when variables is NULL or empty", {
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  # Try to add a table with NULL variables:
  expect_warning(
    out <- remove_table_from_model(
      model = mod,
      variables = NULL,
      firstonly = FALSE,
      file = "emptytab"
    ),
    "No variables to add to \\$TABLE, skipping"
  )
  
  # Verify no table was added:
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("emptytab" %in% out_tables)
  
  # Model should be returned unchanged:
  expect_equal(out$code, mod$code)
  
  # Try to add a table with empty variables:
  expect_warning(
    out <- remove_table_from_model(
      model = mod,
      variables = character(0),
      firstonly = FALSE,
      file = "emptytab2"
    ),
    "No variables to add to \\$TABLE, skipping"
  )
  
  # Verify no table was added:
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("emptytab2" %in% out_tables)
  
  # Model should be returned unchanged:
  expect_equal(out$code, mod$code)
})

test_that("returns model unchanged for non-NONMEM models", {
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
  mod <- create_model(route = "iv", data = dat, tool = "nlmixr2", verbose = FALSE)
  out <- remove_table_from_model(
    model = mod,
    variables = c("ID", "CL"),
    firstonly = FALSE,
    file = "nlmixrtab"
  )
  
  expect_equal(out$code, mod$code)
})

test_that("adds multiple tables sequentially", {
  skip_on_ci()
  # TODO: is this desired behaviour, or should the first table be removed?
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
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  # Add first table:
  mod <- remove_table_from_model(
    model = mod,
    variables = c("ID", "TIME"),
    firstonly = FALSE,
    file = "table1"
  )
  
  # Add second table:
  mod <- remove_table_from_model(
    model = mod,
    variables = c("CL", "V"),
    firstonly = FALSE,
    file = "table2"
  )
  
  # Verify both tables were added:
  out_tables <- get_tables_in_model_code(mod$code)
  expect_true("table1" %in% out_tables)
  expect_true("table2" %in% out_tables)
  expect_length(out_tables, 2)
})
