# TODO: full integration test for create_vpc_data() requires a working
# NONMEM/Pharmpy install. Add it once a skip_no_nonmem() helper exists.
# The tests below cover the pure-R helpers introduced when create_vpc_data()
# was rewritten to avoid Pharmpy serialisation issues with character-typed
# dataset columns.

test_that("create_vpc_data() requires either model or fit object", {
  local_pharmr.extra_options()
  expect_error(
    create_vpc_data(fit = NULL, model = NULL),
    "Either a `fit` object with a model attached, or a `model` argument is required"
  )

  fit_no_model <- list(parameter_estimates = c(CL = 5, V = 50))
  expect_error(
    create_vpc_data(fit = fit_no_model, model = NULL),
    "Either a `fit` object with a model attached, or a `model` argument is required"
  )
})


# get_model_code_pure ----------------------------------------------------

test_that("get_model_code_pure() reads from a file path", {
  tmp <- tempfile(fileext = ".mod")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("$PROBLEM test", "$DATA foo.csv"), tmp)

  out <- pharmr.extra:::get_model_code_pure(tmp)
  expect_true(grepl("\\$PROBLEM test", out))
  expect_true(grepl("\\$DATA foo.csv", out))
})

test_that("get_model_code_pure() returns a code string as-is", {
  code <- "$PROBLEM test\n$DATA foo.csv"
  expect_identical(pharmr.extra:::get_model_code_pure(code), code)
})

test_that("get_model_code_pure() collapses a character vector to one string", {
  code <- c("$PROBLEM test", "$DATA foo.csv")
  out <- pharmr.extra:::get_model_code_pure(code)
  expect_identical(out, "$PROBLEM test\n$DATA foo.csv")
})


# extract_data_path_from_code -------------------------------------------

test_that("extract_data_path_from_code() pulls the first token after $DATA", {
  code <- "$PROBLEM t\n$DATA mydata.csv IGNORE=@\n$INPUT ID DV"
  expect_equal(pharmr.extra:::extract_data_path_from_code(code), "mydata.csv")
})

test_that("extract_data_path_from_code() handles leading whitespace", {
  code <- "$PROBLEM t\n   $DATA  /abs/path.csv\n$INPUT ID DV"
  expect_equal(pharmr.extra:::extract_data_path_from_code(code), "/abs/path.csv")
})

test_that("extract_data_path_from_code() returns NA when no $DATA", {
  code <- "$PROBLEM t\n$INPUT ID DV"
  expect_true(is.na(pharmr.extra:::extract_data_path_from_code(code)))
})


# count_input_columns / get_input_tokens --------------------------------

test_that("count_input_columns() counts tokens across multi-line $INPUT", {
  code <- paste(
    "$PROBLEM t",
    "$INPUT C DROP PROT STUDY2 CENT NSID IDX ID AMT DOSE",
    "       FOOD COH SEQ DROP CLOCK=DROP TAFR TIME TAD",
    "$DATA foo.csv",
    sep = "\n"
  )
  expect_equal(pharmr.extra:::count_input_columns(code), 18)
})

test_that("count_input_columns() stops at the next record keyword", {
  code <- "$INPUT ID TIME DV AMT EVID\n$DATA foo.csv\n$INPUT WRONG"
  expect_equal(pharmr.extra:::count_input_columns(code), 5)
})

test_that("get_input_tokens() returns tokens including =DROP markers", {
  code <- "$INPUT ID TIME DV CLOCK=DROP AMT\n$DATA foo.csv"
  tokens <- pharmr.extra:::get_input_tokens(code)
  expect_equal(tokens, c("ID", "TIME", "DV", "CLOCK=DROP", "AMT"))
})


# sanitize_csv_for_nonmem -----------------------------------------------

test_that("sanitize_csv_for_nonmem() drops trailing non-numeric columns past $INPUT", {
  csv_in <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_in), add = TRUE)
  utils::write.csv(
    data.frame(
      ID = 1:3, TIME = c(0, 1, 2), DV = c(0, 5, 4),
      UNIT = c("ng/mL", "ng/mL", "ng/mL"),
      LABEL = c("x", "y", "z")
    ),
    csv_in, row.names = FALSE
  )
  code <- "$INPUT ID TIME DV\n$DATA foo.csv"

  csv_out <- pharmr.extra:::sanitize_csv_for_nonmem(csv_in, code)
  df <- utils::read.csv(csv_out, check.names = FALSE)
  expect_setequal(names(df), c("ID", "TIME", "DV"))
  expect_true(all(vapply(df, is.numeric, logical(1))))
})

test_that("sanitize_csv_for_nonmem() numerifies string columns within $INPUT range", {
  csv_in <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_in), add = TRUE)
  utils::write.csv(
    data.frame(
      C = c("C", "", ""),                       # comment-flag, non-numeric
      ID = 1:3,
      DATE = c("08/12/2011", "08/13/2011", "08/14/2011"),
      TIME = c(0, 1, 2),
      DV = c(0, 5, 4)
    ),
    csv_in, row.names = FALSE
  )
  code <- "$INPUT C DROP DATE TIME DV\n$DATA foo.csv"

  csv_out <- pharmr.extra:::sanitize_csv_for_nonmem(csv_in, code)
  df <- utils::read.csv(csv_out, check.names = FALSE)
  expect_equal(ncol(df), 5)
  expect_true(all(vapply(df, is.numeric, logical(1))))
  expect_true(all(df$DATE == 0))                # unparseable strings -> 0
  expect_equal(df$TIME, c(0, 1, 2))             # numeric col untouched
})

test_that("sanitize_csv_for_nonmem() fills NA tokens with 0", {
  csv_in <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_in), add = TRUE)
  writeLines(
    c("ID,TIME,DV",
      "1,0,.",
      "1,1,NA",
      "1,2,5"),
    csv_in
  )
  code <- "$INPUT ID TIME DV\n$DATA foo.csv"

  csv_out <- pharmr.extra:::sanitize_csv_for_nonmem(csv_in, code)
  df <- utils::read.csv(csv_out, check.names = FALSE)
  expect_equal(df$DV, c(0, 0, 5))
})


# fix_input_after_set_dataset -------------------------------------------

test_that("fix_input_after_set_dataset() swaps TIME and TAFD when both present", {
  code <- paste(
    "$PROBLEM t",
    "$INPUT ID TIME PERD TAFD DV",
    "$PK",
    "$ERROR\nY = F + EPS(1)",
    sep = "\n"
  )
  out <- pharmr.extra:::fix_input_after_set_dataset(code, csv_path = NULL)
  tokens <- pharmr.extra:::get_input_tokens(out)
  expect_equal(tokens, c("ID", "CLOCK=DROP", "PERD", "TIME", "DV"))
})

test_that("fix_input_after_set_dataset() swaps DV and LNDV only for LTBS models", {
  ltbs_code <- paste(
    "$PROBLEM t",
    "$INPUT ID TIME DV LNDV",
    "$PK",
    "$ERROR\nIPRE = LOG(F)\nY = IPRE + EPS(1)",
    sep = "\n"
  )
  out <- pharmr.extra:::fix_input_after_set_dataset(ltbs_code, csv_path = NULL)
  expect_equal(
    pharmr.extra:::get_input_tokens(out),
    c("ID", "TIME", "CONC=DROP", "DV")
  )

  non_ltbs_code <- paste(
    "$PROBLEM t",
    "$INPUT ID TIME DV LNDV",
    "$PK",
    "$ERROR\nY = F + EPS(1)",
    sep = "\n"
  )
  out2 <- pharmr.extra:::fix_input_after_set_dataset(non_ltbs_code, csv_path = NULL)
  expect_equal(
    pharmr.extra:::get_input_tokens(out2),
    c("ID", "TIME", "DV", "LNDV")
  )
})

test_that("fix_input_after_set_dataset() is a no-op when no swap conditions hit", {
  code <- "$PROBLEM t\n$INPUT ID TIME DV\n$PK\n$ERROR\nY = F + EPS(1)"
  expect_identical(
    pharmr.extra:::fix_input_after_set_dataset(code, csv_path = NULL),
    code
  )
})


# remove_record / add_table_record --------------------------------------

test_that("remove_record() strips a record and all of its continuation lines", {
  code <- paste(
    "$PROBLEM t",
    "$INPUT ID DV",
    "$TABLE ID TIME PRED",
    "       DV EVID",
    "       NOAPPEND NOPRINT FILE=sdtab",
    "$THETA 1",
    sep = "\n"
  )
  out <- pharmr.extra:::remove_record(code, "TABLE")
  expect_false(grepl("\\$TABLE", out))
  expect_false(grepl("NOAPPEND", out))
  expect_true(grepl("\\$THETA 1", out))
})

test_that("remove_record() removes every occurrence", {
  code <- "$TABLE A\n$TABLE B\n$THETA 1\n$TABLE C"
  out <- pharmr.extra:::remove_record(code, "TABLE")
  expect_false(grepl("\\$TABLE", out))
  expect_true(grepl("\\$THETA", out))
})

test_that("add_table_record() appends a single $TABLE record", {
  code <- "$PROBLEM t\n$THETA 1\n"
  out <- pharmr.extra:::add_table_record(code, c("ID", "TIME", "PRED"), file = "sdtab")
  expect_match(out, "\\$TABLE ID TIME PRED NOAPPEND NOPRINT FILE=sdtab")
})


# set_estimation_maxeval_zero / convert_estimation_to_simulation -------

test_that("set_estimation_maxeval_zero() rewrites an existing MAXEVAL", {
  code <- "$EST METHOD=COND INTER MAXEVAL=9990"
  out <- pharmr.extra:::set_estimation_maxeval_zero(code)
  expect_match(out, "MAXEVAL=0")
  expect_false(grepl("MAXEVAL=9990", out))
})

test_that("set_estimation_maxeval_zero() inserts MAXEVAL when absent", {
  code <- "$ESTIMATION METHOD=COND INTER PRINT=10"
  out <- pharmr.extra:::set_estimation_maxeval_zero(code)
  expect_match(out, "MAXEVAL=0")
  expect_match(out, "METHOD=COND")
})

test_that("set_estimation_maxeval_zero() adds an $EST record if none exists", {
  code <- "$PROBLEM t\n$INPUT ID DV\n$THETA 1\n"
  out <- pharmr.extra:::set_estimation_maxeval_zero(code)
  expect_match(out, "\\$ESTIMATION.*MAXEVAL=0")
})

test_that("convert_estimation_to_simulation() replaces $EST with $SIMULATION", {
  code <- "$PROBLEM t\n$EST METHOD=COND INTER MAXEVAL=9990\n$THETA 1"
  out <- pharmr.extra:::convert_estimation_to_simulation(code, n = 50)
  expect_false(grepl("\\$EST(IMATION)?\\b", out))
  expect_match(out, "\\$SIMULATION \\([0-9]+\\) ONLYSIM NSUB=50")
})


# apply_parameters_to_code ---------------------------------------------

test_that("apply_parameters_to_code() updates a (low,init,up) THETA", {
  code <- "$THETA (0,0.1,10)\n$OMEGA 0.1\n$SIGMA 1"
  out <- pharmr.extra:::apply_parameters_to_code(
    code, list(THETA_1 = 0.25, OMEGA_1_1 = 0.2, SIGMA_1_1 = 1.5)
  )
  expect_match(out, "\\(0,0.25,10\\)")
  expect_match(out, "\\$OMEGA 0.2")
  expect_match(out, "\\$SIGMA 1.5")
})

test_that("apply_parameters_to_code() preserves FIX inside a (0 FIX) singleton", {
  # Regression: an earlier version of fmt() returned the empty string for 0,
  # which corrupted (0 FIX) into ( FIX) and broke NONMEM parsing.
  code <- "$THETA (0 FIX) ; CLHEPI1"
  out <- pharmr.extra:::apply_parameters_to_code(code, list(THETA_1 = 0))
  expect_match(out, "\\(0 FIX\\)")
})

test_that("apply_parameters_to_code() handles bare-value THETAs with FIX", {
  code <- "$THETA 0.230 FIX ; Q"
  out <- pharmr.extra:::apply_parameters_to_code(code, list(THETA_1 = 0.42))
  expect_match(out, "0.42 FIX")
})

test_that("apply_parameters_to_code() walks $OMEGA BLOCK init values in order", {
  code <- paste(
    "$OMEGA BLOCK(2)",
    " 0.10",
    " 0.02 0.20",
    "$SIGMA 1",
    sep = "\n"
  )
  out <- pharmr.extra:::apply_parameters_to_code(
    code,
    list(OMEGA_1_1 = 0.5, OMEGA_2_1 = 0.05, OMEGA_2_2 = 0.6, SIGMA_1_1 = 1)
  )
  expect_match(out, "BLOCK\\(2\\)")
  expect_match(out, "0.5\\s")
  expect_match(out, "0.05 0.6")
})

test_that("apply_parameters_to_code() leaves $THETA alone when no THETAs in params", {
  code <- "$THETA (0,0.1,10)\n$OMEGA 0.1"
  out <- pharmr.extra:::apply_parameters_to_code(
    code, list(OMEGA_1_1 = 0.5)
  )
  expect_match(out, "\\(0,0.1,10\\)")
  expect_match(out, "\\$OMEGA 0.5")
})

test_that("apply_parameters_to_code() processes THETAs across multiple $THETA records", {
  code <- paste(
    "$THETA (0,1) ; CL",
    " (0,2) ; V",
    "$OMEGA 0.1",
    "$THETA 3 FIX ; tag",
    sep = "\n"
  )
  out <- pharmr.extra:::apply_parameters_to_code(
    code, list(THETA_1 = 10, THETA_2 = 20, THETA_3 = 30, OMEGA_1_1 = 0.1)
  )
  expect_match(out, "\\(0,10\\)")
  expect_match(out, "\\(0,20\\)")
  expect_match(out, "30 FIX")
})


# replace_inits_in_text -------------------------------------------------

test_that("replace_inits_in_text() in paren mode picks the right index", {
  # Triple -> init at index 2
  out1 <- pharmr.extra:::replace_inits_in_text(
    "(0,0.1,10)", list(0.5), init_in_paren = TRUE
  )
  expect_equal(out1$text, "(0,0.5,10)")

  # Pair -> init at index 2
  out2 <- pharmr.extra:::replace_inits_in_text(
    "(0,0.1)", list(0.5), init_in_paren = TRUE
  )
  expect_equal(out2$text, "(0,0.5)")

  # Singleton -> init at index 1
  out3 <- pharmr.extra:::replace_inits_in_text(
    "(0.1)", list(0.5), init_in_paren = TRUE
  )
  expect_equal(out3$text, "(0.5)")
})

test_that("replace_inits_in_text() in non-paren mode walks every number", {
  out <- pharmr.extra:::replace_inits_in_text(
    " 0.10 0.20 0.30 ; comment-free",
    list(1, 2, 3), init_in_paren = FALSE
  )
  expect_equal(out$text, " 1 2 3 ; comment-free")
  expect_length(out$queue, 0)
})

test_that("replace_inits_in_text() stops when the queue is exhausted", {
  out <- pharmr.extra:::replace_inits_in_text(
    " 0.10 0.20 0.30",
    list(1, 2), init_in_paren = FALSE
  )
  expect_equal(out$text, " 1 2 0.30")
})
