test_that("updates estimation method for all valid methods", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  # Test all allowed methods:
  allowed_methods <- c("FO", "FOCE", "ITS", "IMPMAP", "IMP", "SAEM")
  
  for (method in allowed_methods) {
    updated_mod <- update_estimation_method(mod, method, verbose = FALSE)
    steps <- updated_mod$execution_steps$to_dataframe()
    expect_true(
      tolower(method) %in% tolower(steps$method),
      info = paste("Failed for method:", method)
    )
    expect_s3_class(updated_mod, "pharmpy.model.external.nonmem.model.Model")
  }
})

test_that("input is not case sensitive", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  # lowercase:
  updated_mod <- update_estimation_method(mod, "foce", verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))
  
  # mixed case:
  updated_mod <- update_estimation_method(mod, "FoCe", verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(steps$method))
})

test_that("updates estimation method when model already has one", {
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
    route = "iv", 
    data = dat, 
    estimation_method = "foce",
    verbose = FALSE
  )
  
  # Verify initial method:
  initial_steps <- mod$execution_steps$to_dataframe()
  expect_true("foce" %in% tolower(initial_steps$method))
  
  # Update to different method:
  updated_mod <- update_estimation_method(mod, "SAEM", verbose = FALSE)
  updated_steps <- updated_mod$execution_steps$to_dataframe()
  expect_true("saem" %in% tolower(updated_steps$method))
})

test_that("works with different route types", {
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
  
  # IV model:
  mod_iv <- create_model(route = "iv", data = dat, verbose = FALSE)
  updated_iv <- update_estimation_method(mod_iv, "ITS", verbose = FALSE)
  steps_iv <- updated_iv$execution_steps$to_dataframe()
  expect_true("its" %in% tolower(steps_iv$method))
  
  # oral model:
  mod_oral <- create_model(route = "oral", data = dat, verbose = FALSE)
  updated_oral <- update_estimation_method(mod_oral, "ITS", verbose = FALSE)
  steps_oral <- updated_oral$execution_steps$to_dataframe()
  expect_true("its" %in% tolower(steps_oral$method))
})

test_that("supports multiple estimation steps", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)

  updated_mod <- update_estimation_method(mod, c("SAEM", "IMP"), verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()

  expect_equal(nrow(steps), 2)
  expect_equal(tolower(steps$method[1]), "saem")
  expect_equal(tolower(steps$method[2]), "imp")
})

test_that("reduces to fewer steps when new set is smaller", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  mod <- update_estimation_method(mod, c("SAEM", "IMP"), verbose = FALSE)

  # Now reduce back to a single step
  updated_mod <- update_estimation_method(mod, "FOCE", verbose = FALSE)
  steps <- updated_mod$execution_steps$to_dataframe()

  expect_equal(nrow(steps), 1)
  expect_equal(tolower(steps$method[1]), "foce")
})

test_that("errors on empty estimation method vector", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)

  expect_error(
    update_estimation_method(mod, character(0), verbose = FALSE),
    "At least one estimation method must be provided"
  )
})

test_that("errors on invalid estimation method", {
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
  mod <- create_model(route = "iv", data = dat, verbose = FALSE)
  
  expect_error(
    update_estimation_method(mod, "INVALID", verbose = FALSE),
    "The requested estimation method was not recognized"
  )
  
  expect_error(
    update_estimation_method(mod, "BOGUS", verbose = FALSE),
    "The requested estimation method was not recognized"
  )
})

# -- $TABLE preservation tests ------------------------------------------------

# Helper: build a model with multiple $TABLE blocks (mimics real-world PK models)
make_model_with_multiple_tables <- function() {
  code <- paste0(
    "$PROBLEM Test\n",
    "$INPUT ID TIME DV AMT EVID MDV CMT\n",
    "$DATA data.csv IGNORE=@\n",
    "$SUBROUTINE ADVAN2 TRANS2\n",
    "$PK\n",
    "CL=THETA(1)*EXP(ETA(1))\n",
    "V=THETA(2)*EXP(ETA(2))\n",
    "KA=THETA(3)\n",
    "S2=V\n",
    "$ERROR\n",
    "IPRED=F\n",
    "Y=F+F*EPS(1)\n",
    "$THETA (0,10) ; CL\n",
    "$THETA (0,50) ; V\n",
    "$THETA (0,1)  ; KA\n",
    "$OMEGA 0.1\n",
    "$OMEGA 0.1\n",
    "$SIGMA 0.1\n",
    "$ESTIMATION METHOD=1 INTER MAXEVAL=9999 NOABORT\n",
    "$COVARIANCE PRINT=E\n",
    "$TABLE ID TIME DV PRED IPRED RES WRES ",
    "NOAPPEND ONEHEADER NOPRINT FORMAT=s1PE15.8 FILE=sdtab1\n",
    "$TABLE ID CL V KA ",
    "NOAPPEND ONEHEADER NOPRINT FORMAT=s1PE15.8 FILE=patab1\n",
    "$TABLE ID AMT EVID ",
    "NOAPPEND ONEHEADER NOPRINT FORMAT=s1PE15.8 FILE=cotab1\n"
  )
  pharmr::read_model_from_string(code)
}

test_that("$TABLE records are preserved when updating estimation method", {
  local_pharmr.extra_options()
  mod <- make_model_with_multiple_tables()

  tables_before <- get_tables_in_model_code(mod$code)
  updated_mod <- update_estimation_method(mod, "FOCE", verbose = FALSE)
  tables_after <- get_tables_in_model_code(updated_mod$code)

  # Same number of $TABLE blocks

  expect_equal(length(tables_after), length(tables_before))
  # Same FILE names
  expect_equal(tables_after, tables_before)
})

test_that("$TABLE records are preserved when changing estimation method", {
  local_pharmr.extra_options()
  mod <- make_model_with_multiple_tables()

  tables_before <- get_tables_in_model_code(mod$code)
  updated_mod <- update_estimation_method(mod, "IMP", verbose = FALSE)
  tables_after <- get_tables_in_model_code(updated_mod$code)

  expect_equal(length(tables_after), length(tables_before))
  expect_equal(tables_after, tables_before)
})

test_that("$TABLE content is not corrupted by estimation method update", {
  local_pharmr.extra_options()
  mod <- make_model_with_multiple_tables()

  # Extract original $TABLE blocks (drop empty trailing lines)
  obj_before <- nm_read_model(code = mod$code)
  table_text_before <- obj_before$TABLE
  table_text_before <- table_text_before[nzchar(trimws(table_text_before))]

  updated_mod <- update_estimation_method(mod, "FOCE", verbose = FALSE)

  obj_after <- nm_read_model(code = updated_mod$code)
  table_text_after <- obj_after$TABLE
  table_text_after <- table_text_after[nzchar(trimws(table_text_after))]

  # The $TABLE content should be identical
  expect_equal(table_text_after, table_text_before)
})

test_that("predictions/residuals are not duplicated into wrong $TABLE", {
  local_pharmr.extra_options()
  mod <- make_model_with_multiple_tables()

  updated_mod <- update_estimation_method(mod, "FOCE", verbose = FALSE)

  # The last table (cotab1) should NOT have PRED/IPRED/RES/WRES added to it
  obj <- nm_read_model(code = updated_mod$code)
  table_lines <- obj$TABLE
  # Find cotab1 lines (after the last $TABLE that has FILE=cotab1)
  cotab_start <- max(which(grepl("cotab1", table_lines)))
  cotab_text <- paste(table_lines[cotab_start:length(table_lines)], collapse = " ")

  expect_false(grepl("\\bPRED\\b", cotab_text))
  expect_false(grepl("\\bIPRED\\b", cotab_text))
  expect_false(grepl("\\bRES\\b", cotab_text))
  expect_false(grepl("\\bWRES\\b", cotab_text))
})

test_that("$TABLE preserved with multiple estimation steps", {
  local_pharmr.extra_options()
  mod <- make_model_with_multiple_tables()

  tables_before <- get_tables_in_model_code(mod$code)
  updated_mod <- update_estimation_method(
    mod, c("SAEM", "IMP"), verbose = FALSE
  )
  tables_after <- get_tables_in_model_code(updated_mod$code)

  expect_equal(length(tables_after), length(tables_before))
  expect_equal(tables_after, tables_before)
})
