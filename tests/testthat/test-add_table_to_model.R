test_that("adds table correctly", {
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
  
  # Test basic functionality
  result <- add_table_to_model(
    model = mod,
    variables = c("ID", "CL", "V"),
    firstonly = FALSE,
    file = "patab"
  )
  
  expected_addition <- "\\n\\$TABLE\\n  ID CL V\\n  NOAPPEND NOPRINT\\n  FILE=patab\\n\\n"
  expect_true(grepl(expected_addition, result$code))
  
  # Test with firstonly = TRUE
  result <- add_table_to_model(
    model = mod,
    variables = c("ID", "CL", "V"),
    firstonly = TRUE,
    file = "patab"
  )
  
  expected_addition <- "\\n\\$TABLE\\n  ID CL V\\n  FIRSTONLY\\n  NOAPPEND NOPRINT\\n  FILE=patab\\n\\n"
  expect_true(grepl(expected_addition, result$code))
})

test_that("warns on duplicate file", {
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
    route = "iv", data = dat, tables = "parameters", verbose = FALSE
  )
  
  # Test warning is issued for duplicate file (patab is already in model)
  expect_warning(
    add_table_to_model(
      model = mod,
      variables = c("ID", "CL", "V"),
      firstonly = FALSE,
      file = "patab"
    ),
    "Table file already in a \\$TABLE record in model"
  )
})

test_that("handles empty variables", {
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
  
  # Test with empty variables vector
  expect_warning(
    result <- add_table_to_model(
      model = mod,
      variables = character(0),
      firstonly = FALSE,
      file = "patab"
    )
  )
  expect_equal(result$code, mod$code)
})

test_that("reload_dataset controls whether dataset is reattached", {
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
  expect_false(is.null(mod$dataset))

  # Default (reload_dataset = TRUE): dataset is reattached
  out_true <- add_table_to_model(
    model = mod,
    variables = c("ID", "CL", "V"),
    file = "patab"
  )
  expect_equal(out_true$dataset, mod$dataset, ignore_attr = TRUE)
  expect_true(grepl("FILE=patab", out_true$code))

  # reload_dataset = FALSE: skip the set_dataset() call; table still added
  out_false <- add_table_to_model(
    model = mod,
    variables = c("ID", "CL", "V"),
    file = "patab",
    reload_dataset = FALSE
  )
  expect_null(out_false$dataset)
  expect_true(grepl("FILE=patab", out_false$code))
})

test_that("errors on invalid variables", {
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
  
  # Basic test:
  expect_error(
    add_table_to_model(
      model = mod,
      variables = c(
        "ID", "TIME", "DV", "EVID", "IPRED", "PRED", # valid
        "NOPE", "WRONG" # invalid
      ),
      firstonly = FALSE,
      file = "patab"
    ),
    "NOPE and WRONG are not valid variables"
  )
  
  # Basic ETAs test:
  expect_error(
    add_table_to_model(
      model = mod,
      variables = c(
        "ID", "TIME", "DV", "EVID", "IPRED", "PRED", "ETA1", "ETA2", # valid
        "ETA3" # invalid
      ),
      firstonly = FALSE,
      file = "patab"
    ),
    "ETA3 is not a valid variable"
  )
  
  expect_error(
    add_table_to_model(
      model = mod,
      variables = c(
        "ID", "TIME", "DV", "EVID", "IPRED", "PRED", "ETA(1)", "ETA(2)", # valid
        "ETA(3)" # invalid
      ),
      firstonly = FALSE,
      file = "patab"
    ),
    "ETA\\(3\\) is not a valid variable"
  )
})
