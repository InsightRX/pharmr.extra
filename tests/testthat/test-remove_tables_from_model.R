test_that("removes all tables from model with multiple tables", {
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
  
  # Model has tables initially:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_true("patab" %in% existing_tables)
  expect_true("sdtab" %in% existing_tables)
  expect_length(existing_tables, 2)
  
  # Remove all tables:
  out <- remove_tables_from_model(model = mod, file = NULL)
  out_tables <- get_tables_in_model_code(out$code)
  expect_length(out_tables, 0)
  expect_false(grepl("\\$TABLE", out$code))
  
  # Class is preserved:
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("removes specific table when file is specified", {
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
  
  # Remove only patab:
  out <- remove_tables_from_model(model = mod, file = "patab")
  
  # Verify only patab was removed:
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
  expect_length(out_tables, 1)
  expect_false(grepl("FILE=patab", out$code))
  expect_true(grepl("FILE=sdtab", out$code))
})

test_that("removes table by partial filename match", {
  # TODO: consider whether this is desirable behaviour (probably not)
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
  mod <- create_model(route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE)
  mod <- add_table_to_model(
    model = mod,
    variables = c("ID", "TIME"),
    firstonly = FALSE,
    file = "patab_custom"
  )
  
  # Remove tables starting with "patab":
  out <- remove_tables_from_model(model = mod, file = "patab")
  
  # Verify patab and patab_custom were removed, but sdtab remains:
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("patab" %in% out_tables)
  expect_false("patab_custom" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
})

test_that("handles model with custom tables", {
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

  # Add multiple custom tables:
  mod <- add_table_to_model(
    model = mod,
    variables = c("ID", "TIME"),
    firstonly = FALSE,
    file = "table1"
  )
  mod <- add_table_to_model(
    model = mod,
    variables = c("CL", "V"),
    firstonly = FALSE,
    file = "table2"
  )
  mod <- add_table_to_model(
    model = mod,
    variables = c("DV", "PRED"),
    firstonly = FALSE,
    file = "table3"
  )
  
  # Verify all tables exist:
  existing_tables <- get_tables_in_model_code(mod$code)
  expect_length(existing_tables, 3)
  
  # Remove all tables:
  out <- remove_tables_from_model(model = mod, file = NULL)
  
  # Verify all tables were removed:
  out_tables <- get_tables_in_model_code(out$code)
  expect_length(out_tables, 0)
  expect_false(grepl("\\$TABLE", out$code))
})

test_that("removes only matching table when file is specified", {
  # TODO: This currently fails due to partial matching (see above) 
  skip()
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
  
  # Add tables with similar names:
  mod <- add_table_to_model(
    model = mod,
    variables = c("ID", "TIME"),
    firstonly = FALSE,
    file = "mytab"
  )
  mod <- add_table_to_model(
    model = mod,
    variables = c("CL", "V"),
    firstonly = FALSE,
    file = "mytab2"
  )
  mod <- add_table_to_model(
    model = mod,
    variables = c("DV", "PRED"),
    firstonly = FALSE,
    file = "othertab"
  )
  
  # Remove only mytab:
  out <- remove_tables_from_model(model = mod, file = "mytab")
  
  # Verify only mytab was removed (exact match, not prefix):
  out_tables <- get_tables_in_model_code(out$code)
  expect_false("mytab" %in% out_tables)
  expect_true("mytab2" %in% out_tables)
  expect_true("othertab" %in% out_tables)
  expect_length(out_tables, 2)
})

test_that("returns model unchanged when no tables exist", {
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
  out <- remove_tables_from_model(model = mod, file = NULL)
  expect_equal(out, mod)
})

test_that("returns model unchanged for non-NONMEM models", {
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
  out <- remove_tables_from_model(model = mod, file = NULL)
  expect_equal(out, mod)
})

test_that("preserves datainfo column types when removing tables", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = c(100, 0, 0, 100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 1, 0, 0),
    MDV = c(1, 0, 0, 1, 0, 0)
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )

  out <- remove_tables_from_model(model = mod, file = NULL)

  # datainfo types must be preserved (not reset to "unknown")
  expect_false(is.null(out$datainfo))
  expect_equal(out$datainfo[["ID"]]$type, "id")
  expect_equal(out$datainfo[["DV"]]$type, "dv")
})

test_that("preserves dataset when removing tables", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = c(100, 0, 0, 100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 1, 0, 0),
    MDV = c(1, 0, 0, 1, 0, 0)
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  out <- remove_tables_from_model(model = mod, file = NULL)
  expect_equal(out$dataset, mod$dataset, ignore_attr = TRUE)
})

test_that("preserves non-numeric DROP columns when removing tables (#99)", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = c(100, 0, 0, 100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 1, 0, 0),
    MDV = c(1, 0, 0, 1, 0, 0),
    # Non-numeric date/time-of-day columns NONMEM ignores via $INPUT DROP.
    # These crash if the dataset round-trip loses the DROP flag and pharmpy
    # tries to float-convert them.
    VISITDATE = c("08/12/2011", "08/13/2011", "08/14/2011",
                  "09/01/2011", "09/02/2011", "09/03/2011"),
    CLOCK = c("9:00", "10:00", "11:00", "9:00", "10:00", "11:00")
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"),
    drop_input = c("VISITDATE", "CLOCK"), verbose = FALSE
  )

  # Removing tables must not raise a DatasetError on the non-numeric columns:
  out <- expect_no_error(remove_tables_from_model(model = mod, file = NULL))

  expect_length(get_tables_in_model_code(out$code), 0)
  # Dataset is still materializable and keeps the non-numeric columns intact:
  expect_false(is.null(out$dataset))
  expect_true(all(c("VISITDATE", "CLOCK") %in% names(out$dataset)))
})

test_that("reload_dataset = FALSE skips reattaching dataset", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = c(100, 0, 0, 100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0, 1, 0, 0),
    MDV = c(1, 0, 0, 1, 0, 0)
  )
  mod <- create_model(
    route = "iv", data = dat, tables = c("fit", "parameters"), verbose = FALSE
  )
  expect_false(is.null(mod$dataset))

  out <- remove_tables_from_model(model = mod, file = NULL, reload_dataset = FALSE)

  # Tables are still removed from the code...
  expect_length(get_tables_in_model_code(out$code), 0)
  expect_false(grepl("\\$TABLE", out$code))
  # ...but the dataset is not reattached.
  expect_null(out$dataset)
})
