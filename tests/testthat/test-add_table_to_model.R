test_that("add_table_to_model adds table correctly", {
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

test_that("add_table_to_model warns on duplicate file", {
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

test_that("add_table_to_model handles empty variables", {
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
