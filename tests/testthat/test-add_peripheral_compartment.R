dat_iv <- data.frame(
  ID = 1,
  TIME = c(0, 1, 2, 3, 4),
  DV = c(0, 10, 5, 3, 2),
  AMT = c(100, 0, 0, 0, 0),
  CMT = 1,
  EVID = c(1, 0, 0, 0, 0),
  MDV = c(1, 0, 0, 0, 0)
)

dat_oral <- data.frame(
  ID = 1,
  TIME = c(0, 1, 2, 3, 4),
  DV = c(0, 10, 5, 3, 2),
  AMT = c(100, 0, 0, 0, 0),
  CMT = 1,
  EVID = c(1, 0, 0, 0, 0),
  MDV = c(1, 0, 0, 0, 0)
)

test_that("returns a model of the correct class", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", data = dat_iv, tables = NULL, verbose = FALSE)
  out <- add_peripheral_compartment(mod)
  expect_s3_class(out, "pharmpy.model.external.nonmem.model.Model")
})

test_that("fixes patab $TABLE references after adding peripheral compartment (IV oral)", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "oral", data = dat_oral, tables = "parameters", verbose = FALSE
  )

  # 1-cmt model patab has V in $TABLE
  expect_true("patab" %in% get_tables_in_model_code(mod$code))

  mod_2cmt <- add_peripheral_compartment(mod)

  # patab should still be present
  expect_true("patab" %in% get_tables_in_model_code(mod_2cmt$code))

  # Extract just $TABLE section(s) and verify bare V is gone
  table_section <- stringr::str_extract_all(mod_2cmt$code, "\\$TABLE[^$]+")[[1]]
  expect_false(any(grepl("\\bV\\b", table_section)))
})

test_that("fixes patab $TABLE references after adding peripheral compartment (IV)", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "iv", data = dat_iv, tables = "parameters", verbose = FALSE
  )

  expect_true("patab" %in% get_tables_in_model_code(mod$code))

  mod_2cmt <- add_peripheral_compartment(mod)

  expect_true("patab" %in% get_tables_in_model_code(mod_2cmt$code))

  table_section <- stringr::str_extract_all(mod_2cmt$code, "\\$TABLE[^$]+")[[1]]
  expect_false(any(grepl("\\bV\\b", table_section)))
})

test_that("does not add patab when model has no tables initially", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", data = dat_iv, tables = NULL, verbose = FALSE)

  expect_length(get_tables_in_model_code(mod$code), 0)

  mod_2cmt <- add_peripheral_compartment(mod)

  expect_length(get_tables_in_model_code(mod_2cmt$code), 0)
})

test_that("preserves sdtab when fixing patab", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "iv", data = dat_iv, tables = c("fit", "parameters"), verbose = FALSE
  )

  existing <- get_tables_in_model_code(mod$code)
  expect_true("patab" %in% existing)
  expect_true("sdtab" %in% existing)

  mod_2cmt <- add_peripheral_compartment(mod)

  out_tables <- get_tables_in_model_code(mod_2cmt$code)
  expect_true("patab" %in% out_tables)
  expect_true("sdtab" %in% out_tables)
})

test_that("works for two sequential calls (3-compartment)", {
  local_pharmr.extra_options()
  mod <- create_model(
    route = "iv", data = dat_iv, tables = "parameters", verbose = FALSE
  )

  mod_3cmt <- mod |>
    add_peripheral_compartment() |>
    add_peripheral_compartment()

  expect_true("patab" %in% get_tables_in_model_code(mod_3cmt$code))

  table_section <- stringr::str_extract_all(mod_3cmt$code, "\\$TABLE[^$]+")[[1]]
  expect_false(any(grepl("\\bV\\b", table_section)))
})
