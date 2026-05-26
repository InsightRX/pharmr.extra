library(mockery)

## TODO: needs tests for main run_nlme function

test_that("run_nlme mu_reference='auto': applies mu-referencing when SAEM + not mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_false(pharmr::has_mu_reference(mod_saem))

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_message(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
    ),
    "mu-referenc"
  )
  expect_true(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference='auto': no message when SAEM already mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = TRUE)
  expect_true(pharmr::has_mu_reference(mod_saem))

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_message(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
    )
  )
})

test_that("run_nlme mu_reference=TRUE: always applies mu-referencing (FOCE model)", {
  local_pharmr.extra_options()
  mod_foce <- create_model(estimation_method = "foce", mu_reference = FALSE)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_message(
    tryCatch(
      run_nlme(mod_foce, id = "run1", path = withr::local_tempdir(),
               mu_reference = TRUE, verbose = FALSE),
      error = function(e) NULL
    ),
    "mu-referenc"
  )
  expect_true(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference=FALSE: warns when SAEM model is not mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = FALSE)
  expect_false(pharmr::has_mu_reference(mod_saem))

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  expect_warning(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = FALSE, verbose = FALSE),
      error = function(e) NULL
    ),
    "not mu-referenced"
  )
  # Model should NOT have been mu-referenced
  expect_false(pharmr::has_mu_reference(captured_model))
})

test_that("run_nlme mu_reference=FALSE: no warning when SAEM already mu-referenced", {
  local_pharmr.extra_options()
  mod_saem <- create_model(estimation_method = "saem", mu_reference = TRUE)

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_warning(
    tryCatch(
      run_nlme(mod_saem, id = "run1", path = withr::local_tempdir(),
               mu_reference = FALSE, verbose = FALSE),
      error = function(e) NULL
    )
  )
})

test_that("run_nlme mu_reference='auto': no warning/message for FOCE model", {
  local_pharmr.extra_options()
  mod_foce <- create_model(estimation_method = "foce")

  stub(run_nlme, "prepare_run_folder", function(...) stop("abort before NONMEM"))

  expect_no_warning(
    tryCatch(
      run_nlme(mod_foce, id = "run1", path = withr::local_tempdir(),
               mu_reference = "auto", verbose = FALSE),
      error = function(e) NULL
    )
  )
})


test_that("get_new_run_number works correctly", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Test 1: Empty directory should return 1
  expect_equal(get_new_run_number(temp_dir), 1)
  
  # Test 2: With existing run folders
  dir.create(file.path(temp_dir, "run1"))
  dir.create(file.path(temp_dir, "run2"))
  expect_equal(get_new_run_number(temp_dir), 3)
  
  # Test 3: Non-sequential numbers
  unlink(file.path(temp_dir, "run2"))
  dir.create(file.path(temp_dir, "run5"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 4: With non-run folders present
  dir.create(file.path(temp_dir, "other_folder"))
  expect_equal(get_new_run_number(temp_dir), 6)
  
  # Test 5: With invalid run folder names
  dir.create(file.path(temp_dir, "runA"))
  dir.create(file.path(temp_dir, "run"))
  expect_equal(get_new_run_number(temp_dir), 6)
})

test_that("change_nonmem_dataset handles different input formats correctly", {
  # Test single-line string input
  model_code_single <- "$PROB TEST\n$DATA old_data.csv IGNORE=@\n$INPUT ID TIME DV"
  result1 <- change_nonmem_dataset(model_code_single, "new_data.csv")
  expect_match(result1, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test vector input
  model_code_vector <- c("$PROB TEST", "$DATA old_data.csv IGNORE=@", "$INPUT ID TIME DV")
  result2 <- change_nonmem_dataset(model_code_vector, "new_data.csv")
  expect_match(result2, "\\$DATA new_data\\.csv IGNORE=@")
  
  # Test with multiple options after dataset
  model_code <- "$PROB TEST\n$DATA old_data.csv IGNORE=@ ACCEPT=(DV.GT.0)\n$INPUT ID TIME DV"
  result3 <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result3, "\\$DATA new_data\\.csv IGNORE=@ ACCEPT=\\(DV\\.GT\\.0\\)")
})

test_that("change_nonmem_dataset handles errors appropriately", {
  # Test missing $DATA line
  model_code_no_data <- "$PROB TEST\n$INPUT ID TIME DV"
  expect_error(
    change_nonmem_dataset(model_code_no_data, "new_data.csv"),
    "No \\$DATA record found"
  )
})

test_that("run_nlme removes tables from model when remove_tables = TRUE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)
  expect_true(length(get_tables_in_model_code(mod$code)) > 0)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             remove_tables = TRUE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(is.null(captured_model))
  expect_length(get_tables_in_model_code(captured_model$code), 0)
})

test_that("run_nlme preserves tables when remove_tables = FALSE", {
  local_pharmr.extra_options()
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = "fit", verbose = FALSE)
  n_tables <- length(get_tables_in_model_code(mod$code))
  expect_true(n_tables > 0)

  captured_model <- NULL
  stub(run_nlme, "prepare_run_folder", function(id, model, ...) {
    captured_model <<- model
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             remove_tables = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(is.null(captured_model))
  expect_length(get_tables_in_model_code(captured_model$code), n_tables)
})

test_that("run_nlme / prepare_run_folder strip surrounding quotes from column names", {
  local_pharmr.extra_options()

  ## Data.frame with column names wrapped in literal quote characters —
  ## Pharmpy rejects these (not valid Python identifiers) and NONMEM cannot
  ## parse a CSV whose header row starts with a quote character.
  df <- data.frame(
    ID = rep(1:2, each = 3),
    TIME = rep(c(0, 1, 2), 2),
    DV = c(0, 10, 5, 0, 12, 6),
    AMT = rep(c(100, 0, 0), 2),
    CMT = 1,
    EVID = rep(c(1, 0, 0), 2),
    MDV = rep(c(1, 0, 0), 2)
  )
  names(df) <- paste0('"', names(df), '"')

  mod <- create_model(route = "iv", verbose = FALSE)
  td <- withr::local_tempdir()

  obj <- prepare_run_folder(
    id = "run1", model = mod, path = td, data = df,
    auto_stack_encounters = FALSE, verbose = FALSE
  )

  first_line <- readLines(file.path(obj$fit_folder, "data.csv"), n = 1)
  expect_false(grepl('^"', first_line))
  expect_equal(
    strsplit(first_line, ",", fixed = TRUE)[[1]],
    c("ID", "TIME", "DV", "AMT", "CMT", "EVID", "MDV")
  )

  ## Also covers the filename path: when the source CSV has quoted headers,
  ## the written dataset should have them stripped.
  quoted_csv <- file.path(withr::local_tempdir(), "quoted.csv")
  writeLines(c('"ID","TIME","DV"', "1,0,0", "1,1,10"), quoted_csv)

  obj2 <- prepare_run_folder(
    id = "run1", model = mod, path = withr::local_tempdir(), data = quoted_csv,
    auto_stack_encounters = FALSE, verbose = FALSE
  )
  first_line2 <- readLines(file.path(obj2$fit_folder, "data.csv"), n = 1)
  expect_false(grepl('^"', first_line2))
  expect_equal(
    strsplit(first_line2, ",", fixed = TRUE)[[1]],
    c("ID", "TIME", "DV")
  )
})

test_that("prepare_run_folder respects copy_dataset", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- create_model(route = "iv", verbose = FALSE)

  src_dir <- withr::local_tempdir()
  src_csv <- file.path(src_dir, "mydata.csv")
  writeLines(c("ID,TIME,DV", "1,0,0", "1,1,10"), src_csv)

  ## copy_dataset = FALSE: leave dataset in place, point $DATA at its abs path
  obj_no_copy <- prepare_run_folder(
    id = "run1", model = mod, path = withr::local_tempdir(), data = src_csv,
    copy_dataset = FALSE, verbose = FALSE
  )
  expect_false(file.exists(file.path(obj_no_copy$fit_folder, "data.csv")))
  expect_equal(obj_no_copy$dataset_path, normalizePath(src_csv))
  data_line <- grep("^\\$DATA", readLines(
    file.path(obj_no_copy$fit_folder, obj_no_copy$model_file)
  ), value = TRUE)
  expect_match(data_line, normalizePath(src_csv), fixed = TRUE)

  ## copy_dataset = TRUE: dataset copied into run folder, $DATA points to copy
  obj_copy <- prepare_run_folder(
    id = "run1", model = mod, path = withr::local_tempdir(), data = src_csv,
    copy_dataset = TRUE, verbose = FALSE
  )
  expect_true(file.exists(file.path(obj_copy$fit_folder, "data.csv")))
  expect_equal(
    obj_copy$dataset_path,
    file.path(obj_copy$fit_folder, "data.csv")
  )
})

test_that("unquote_column_names strips a single pair of surrounding quotes", {
  df <- data.frame(a = 1, b = 2, c = 3)
  names(df) <- c('"a"', "'b'", "c")
  out <- unquote_column_names(df)
  expect_equal(names(out), c("a", "b", "c"))

  ## Unmatched or internal quotes left alone
  df2 <- data.frame(x = 1, y = 2, z = 3)
  names(df2) <- c('"x', 'y"', 'a"b')
  out2 <- unquote_column_names(df2)
  expect_equal(names(out2), c('"x', 'y"', 'a"b'))

  ## NULL and non-data.frame input returned unchanged
  expect_null(unquote_column_names(NULL))
  expect_equal(unquote_column_names("not a df"), "not a df")
})

test_that("run_nlme converts data.frame input to a CSV file path", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, data = dat, id = "run1", path = withr::local_tempdir(),
             verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(is.character(captured_data))
  expect_match(captured_data, "\\.csv$")
  expect_true(file.exists(captured_data))

  ## Round-trip: file should contain the same data we passed in
  written <- read.csv(captured_data)
  expect_equal(written, dat, ignore_attr = TRUE)
})

test_that("run_nlme forces copy_dataset for in-memory data.frame input", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  mod <- create_model(route = "iv", verbose = FALSE)
  dat <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 10, 5),
    AMT = c(100, 0, 0), CMT = 1, EVID = c(1, 0, 0), MDV = c(1, 0, 0)
  )

  ## A data.frame has no on-disk location to reference, so even with
  ## copy_dataset = FALSE it must be written into the run folder (otherwise
  ## $DATA would point at an ephemeral tempfile).
  captured_copy <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_copy <<- list(...)$copy_dataset
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, data = dat, id = "run1", path = withr::local_tempdir(),
             copy_dataset = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(captured_copy)
})

test_that("run_nlme passes through a CSV file path unchanged", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)
  csv_path <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(ID = 1, TIME = 0, DV = 0, AMT = 100, CMT = 1, EVID = 1, MDV = 1),
    csv_path, quote = FALSE, row.names = FALSE
  )

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, data = csv_path, id = "run1", path = withr::local_tempdir(),
             verbose = FALSE),
    error = function(e) NULL
  )

  expect_identical(captured_data, csv_path)
})

test_that("run_nlme accepts NULL data (uses dataset embedded in model)", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)

  captured_data <- "<not captured>"
  stub(run_nlme, "prepare_run_folder", function(id, model, path, data, ...) {
    captured_data <<- data
    stop("abort before NONMEM")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(), verbose = FALSE),
    error = function(e) NULL
  )

  expect_null(captured_data)
})

test_that("run_nlme aborts when `data` is neither data.frame, character, nor NULL", {
  local_pharmr.extra_options()
  mod <- create_model(route = "iv", verbose = FALSE)

  expect_error(
    run_nlme(mod, data = list(a = 1), id = "run1",
             path = withr::local_tempdir(), verbose = FALSE),
    "unknown type"
  )
})

test_that("change_nonmem_dataset preserves whitespace and formatting", {
  # Extra whitespace around the path is preserved verbatim
  model_code <- "$PROB TEST\n$DATA    old_data.csv    IGNORE=@   \n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code, "new_data.csv")
  expect_match(result, "\\$DATA    new_data\\.csv    IGNORE=@   ")

  # Tabs are preserved verbatim
  model_code_tabs <- "$PROB TEST\n$DATA\told_data.csv\tIGNORE=@\n$INPUT ID TIME DV"
  result <- change_nonmem_dataset(model_code_tabs, "new_data.csv")
  expect_match(result, "\\$DATA\tnew_data\\.csv\tIGNORE=@")
})

# Helper: stub `prepare_run_folder` to return a fit_folder pointed at a
# temp dir. We pass `nmfe = "/fake/nmfe"` explicitly because the default
# `get_nmfe_location()` reads the pharmpy config and aborts in CI/dev
# machines without one.
make_obj_stub <- function(fit_folder) {
  function(id, model, ...) {
    if(!dir.exists(fit_folder)) dir.create(fit_folder, recursive = TRUE)
    list(
      model = model,
      model_file = "run.mod",
      output_file = "run.lst",
      fit_folder = fit_folder,
      dataset_path = file.path(fit_folder, "data.csv")
    )
  }
}

test_that("run_nlme with threads writes a parafile to the fit folder", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_nmfe", function(...) stop("abort after dispatch"))

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             method = "nmfe", threads = 4, mu_reference = FALSE,
             nmfe = "/fake/nmfe",
             save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
             clean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  parafile_path <- file.path(fit_folder, "parafile.pnm")
  expect_true(file.exists(parafile_path))
  expect_true(any(grepl("\\[nodes\\]=4", readLines(parafile_path))))
})

test_that("run_nlme with method='nmfe' + threads passes -parafile and [nodes] to call_nmfe", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()
  state <- new.env()

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_nmfe", function(...) {
    state$captured <- list(...)
    stop("abort after dispatch")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             method = "nmfe", threads = 3, mu_reference = FALSE,
             nmfe = "/fake/nmfe",
             save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
             clean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(!is.null(state$captured$parafile))
  expect_true(file.exists(state$captured$parafile))
  expect_identical(state$captured$threads, 3)
})

test_that("run_nlme with method='psn' + threads passes parafile and threads to call_psn", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()
  state <- new.env()

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_psn", function(...) {
    state$captured <- list(...)
    stop("abort after dispatch")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             method = "psn", threads = 2, mu_reference = FALSE,
             nmfe = "/fake/nmfe",
             save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
             clean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(!is.null(state$captured$parafile))
  expect_true(file.exists(state$captured$parafile))
  expect_identical(state$captured$threads, 2)
})

test_that("run_nlme with method='pharmpy' + threads warns and falls back to nmfe", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()
  state <- new.env()
  state$nmfe_called <- FALSE
  state$pharmpy_called <- FALSE

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_nmfe", function(...) {
    state$nmfe_called <- TRUE
    stop("abort after dispatch")
  })
  stub(run_nlme, "call_pharmpy_fit", function(...) {
    state$pharmpy_called <- TRUE
    stop("abort after dispatch")
  })

  expect_warning(
    tryCatch(
      run_nlme(mod, id = "run1", path = withr::local_tempdir(),
               method = "pharmpy", threads = 2, mu_reference = FALSE,
               nmfe = "/fake/nmfe",
               save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
               clean = FALSE, verbose = FALSE),
      error = function(e) NULL
    ),
    "Pharmpy backend does not support parafiles"
  )

  expect_true(state$nmfe_called)
  expect_false(state$pharmpy_called)
})

test_that("run_nlme without threads does not write a parafile or pass parafile args", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()
  state <- new.env()

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_nmfe", function(...) {
    state$captured <- list(...)
    stop("abort after dispatch")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             method = "nmfe", mu_reference = FALSE,
             nmfe = "/fake/nmfe",
             save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
             clean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(file.exists(file.path(fit_folder, "parafile.pnm")))
  expect_null(state$captured$parafile)
  expect_null(state$captured$threads)
})

test_that("run_nlme with threads=1 does not write a parafile", {
  local_pharmr.extra_options()
  mod <- make_model_without_cov()
  fit_folder <- withr::local_tempdir()
  state <- new.env()

  stub(run_nlme, "prepare_run_folder", make_obj_stub(fit_folder))
  stub(run_nlme, "call_nmfe", function(...) {
    state$captured <- list(...)
    stop("abort after dispatch")
  })

  tryCatch(
    run_nlme(mod, id = "run1", path = withr::local_tempdir(),
             method = "nmfe", threads = 1, mu_reference = FALSE,
             nmfe = "/fake/nmfe",
             save_fit = FALSE, save_summary = FALSE, save_final = FALSE,
             clean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )

  expect_false(file.exists(file.path(fit_folder, "parafile.pnm")))
  expect_null(state$captured$parafile)
})

test_that("call_nmfe appends -parafile and [nodes] args when parafile supplied", {
  local_pharmr.extra_options()
  tmp <- withr::local_tempdir()
  fake_nmfe <- file.path(tmp, "fake_nmfe")
  writeLines("#!/bin/sh\necho \"$@\" > nmfe_args.txt\nexit 0", fake_nmfe)
  Sys.chmod(fake_nmfe, "0755")

  parafile <- create_mpi_parafile(tmp, threads = 4)

  call_nmfe(
    model_file = "run.mod",
    output_file = "run.lst",
    path = tmp,
    nmfe = fake_nmfe,
    parafile = parafile,
    threads = 4,
    console = TRUE,
    verbose = FALSE
  )

  args_line <- readLines(file.path(tmp, "nmfe_args.txt"))
  expect_match(args_line, "run\\.mod")
  expect_match(args_line, "run\\.lst")
  expect_match(args_line, paste0("-parafile=", parafile), fixed = TRUE)
  expect_match(args_line, "\\[nodes\\]=4")
})

test_that("call_psn injects --parafile and --nodes via parse_psn_args", {
  local_pharmr.extra_options()
  tmp <- withr::local_tempdir()

  ## Stub system2 to capture args without running PsN
  captured_args <- NULL
  stub(call_psn, "system2", function(command, args, ...) {
    captured_args <<- args
    0L
  })

  parafile <- create_mpi_parafile(tmp, threads = 3)

  call_psn(
    model_file = "run.mod",
    output_file = "run.lst",
    path = tmp,
    tool = "execute",
    parafile = parafile,
    threads = 3,
    console = TRUE,
    verbose = FALSE
  )

  ## `args` may be a vector — collapse for the regex check
  joined <- paste(captured_args, collapse = " ")
  expect_true(grepl(paste0("--parafile=", parafile), joined, fixed = TRUE))
  expect_true(grepl("--nodes=3", joined, fixed = TRUE))
})

