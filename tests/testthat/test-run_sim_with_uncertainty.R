# ===========================================================================
# run_sim_with_uncertainty() — test suite
# ===========================================================================
#
# Tests are split into three tiers:
#   1. No external tools required — pure R / file I/O
#   2. read_vpc_sim_tables() helper — temp-directory-based file I/O
#   3. Integration tests with Pharmpy loaded, call_psn() mocked

# ---------------------------------------------------------------------------
# Helpers shared across tiers
# ---------------------------------------------------------------------------

## Write a minimal NONMEM-format table file to disk
.write_nm_table <- function(path, ids = 1:3) {
  rows <- do.call(rbind, lapply(ids, function(i) {
    c(sprintf("%12.4E", as.numeric(i)),
      sprintf("%12.4E", 0),
      sprintf("%12.4E", 5),
      sprintf("%12.4E", 4.9))
  }))
  header <- "TABLE NO.  1\n ID          TIME        DV          PRED"
  body   <- apply(rows, 1, paste, collapse = "  ")
  writeLines(c(header, body), path)
}

## Build a fake PsN vpc output directory with N NM_run subdirectories
.make_fake_vpc_dir <- function(base, n = 3, table_name = "simtab") {
  m1_dir <- file.path(base, "m1")
  for (i in seq_len(n)) {
    run_dir <- file.path(m1_dir, paste0("NM_run", i))
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    .write_nm_table(file.path(run_dir, table_name), ids = 1:2)
  }
  invisible(base)
}

# ===========================================================================
# Tier 1 – No external tools
# ===========================================================================

test_that("run_sim_with_uncertainty: errors when both fit and fit_folder are NULL", {
  expect_error(
    run_sim_with_uncertainty(),
    regexp = "Supply either"
  )
})

# ---------------------------------------------------------------------------
# extract_fit_folder() — private helper
# ---------------------------------------------------------------------------

test_that("extract_fit_folder: returns NULL when fit has no model attribute", {
  fit_no_model <- list()
  expect_null(extract_fit_folder(fit_no_model))
})

test_that("extract_fit_folder: returns NULL when model$path is NULL", {
  fake_model <- list(path = NULL)
  fake_fit   <- structure(list(), model = fake_model)
  attr(fake_fit, "model") <- fake_model
  expect_null(extract_fit_folder(fake_fit))
})

test_that("extract_fit_folder: returns NULL when model$path is a Python NoneType mock", {
  fake_none  <- structure(list(), class = "python.builtin.NoneType")
  fake_model <- list(path = fake_none)
  fake_fit   <- list()
  attr(fake_fit, "model") <- fake_model
  expect_null(extract_fit_folder(fake_fit))
})

# ===========================================================================
# Tier 2 – read_vpc_sim_tables() — temp-directory-based file I/O
# ===========================================================================

test_that("read_vpc_sim_tables: errors when m1/ subdirectory is absent", {
  withr::local_tempdir() -> vpc_dir
  expect_error(
    read_vpc_sim_tables(vpc_dir),
    regexp = "m1.*subdirectory"
  )
})

test_that("read_vpc_sim_tables: errors when no NM_run directories exist in m1/", {
  withr::local_tempdir() -> base_dir
  vpc_dir <- file.path(base_dir, "vpc_out")
  dir.create(file.path(vpc_dir, "m1"), recursive = TRUE)
  expect_error(
    read_vpc_sim_tables(vpc_dir),
    regexp = "NM_run"
  )
})

test_that("read_vpc_sim_tables: returns data.frame with sample_id column", {
  withr::local_tempdir() -> base_dir
  .make_fake_vpc_dir(base_dir, n = 3)
  out <- read_vpc_sim_tables(base_dir)
  expect_s3_class(out, "data.frame")
  expect_true("sample_id" %in% names(out))
  expect_setequal(unique(out$sample_id), 1:3)
})

test_that("read_vpc_sim_tables: sample_id matches NM_run numbering", {
  withr::local_tempdir() -> base_dir
  .make_fake_vpc_dir(base_dir, n = 5)
  out <- read_vpc_sim_tables(base_dir)
  expect_equal(sort(unique(out$sample_id)), 1:5)
})

test_that("read_vpc_sim_tables: numeric sort — NM_run10 sorted after NM_run9", {
  withr::local_tempdir() -> base_dir
  .make_fake_vpc_dir(base_dir, n = 10)
  out <- read_vpc_sim_tables(base_dir)
  expect_equal(sort(unique(out$sample_id)), 1:10)
  ## Verify rows from sample 10 are present (would be dropped if sorted lexicographically
  ## into the wrong position and only 9 samples were read)
  expect_true(10 %in% out$sample_id)
})

test_that("read_vpc_sim_tables: warns and skips when a table file is missing", {
  withr::local_tempdir() -> base_dir
  .make_fake_vpc_dir(base_dir, n = 3)
  # Delete one table to trigger the warning path
  unlink(file.path(base_dir, "m1", "NM_run2", "simtab"))
  # Capture all warnings (function emits two: per-file + summary)
  warned <- character(0)
  withCallingHandlers(
    out <- read_vpc_sim_tables(base_dir),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("not found", warned)))
  # Remaining 2 samples still returned
  expect_s3_class(out, "data.frame")
  expect_setequal(unique(out$sample_id), c(1L, 3L))
})

test_that("read_vpc_sim_tables: errors when all table files are missing", {
  withr::local_tempdir() -> base_dir
  m1_dir <- file.path(base_dir, "m1")
  # Create NM_run dirs but no simtab files
  for (i in 1:2) dir.create(file.path(m1_dir, paste0("NM_run", i)), recursive = TRUE)
  expect_error(
    suppressWarnings(read_vpc_sim_tables(base_dir)),
    regexp = "No simulation tables"
  )
})

test_that("read_vpc_sim_tables: custom table_name is used", {
  withr::local_tempdir() -> base_dir
  .make_fake_vpc_dir(base_dir, n = 2, table_name = "myresults")
  out <- read_vpc_sim_tables(base_dir, table_name = "myresults")
  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$sample_id)), 2)
})

test_that("read_vpc_sim_tables: data from all runs is combined into one data.frame", {
  withr::local_tempdir() -> base_dir
  # Each run has 2 IDs (see .write_nm_table default)
  .make_fake_vpc_dir(base_dir, n = 4)
  out <- read_vpc_sim_tables(base_dir)
  # 4 runs × 2 IDs = 8 unique (ID, sample_id) combinations
  expect_equal(nrow(dplyr::distinct(out, .data$ID, .data$sample_id)), 8)
})

# ===========================================================================
# Tier 3 – Integration: Pharmpy loaded, call_psn() mocked
# ===========================================================================

## Minimal NONMEM model code used in integration tests below
.nm_model_code <- function(data_path = "data.csv") {
  paste0(
    "$PROBLEM Test\n",
    "$INPUT ID TIME DV AMT EVID MDV\n",
    "$DATA ", data_path, " IGNORE=@\n",
    "$SUBROUTINES ADVAN1 TRANS2\n",
    "$PK\nCL=THETA(1)*EXP(ETA(1))\nV=THETA(2)\nS1=V\n",
    "$ERROR\nY=F+EPS(1)\n",
    "$THETA (0,10)\n$THETA (0,50)\n",
    "$OMEGA 0.1\n$SIGMA 0.1\n",
    "$EST METHOD=1\n",
    "$COV UNCOND\n"
  )
}

## Minimal dataset that can sit alongside the model
.write_minimal_dataset <- function(path) {
  dat <- data.frame(
    ID = 1, TIME = c(0, 6, 12, 24),
    DV = c(0, 5, 3, 1), AMT = c(100, 0, 0, 0),
    EVID = c(1, 0, 0, 0), MDV = c(1, 0, 0, 0)
  )
  write.csv(dat, path, quote = FALSE, row.names = FALSE)
}

## Build a minimal .cov file in NONMEM format (2×2 THETA block only)
.write_minimal_cov <- function(path) {
  writeLines(c(
    "TABLE NO.  1: Covariance Matrix",
    " NAME         THETA1       THETA2",
    " THETA1        1.0000E+00  0.0000E+00",
    " THETA2        0.0000E+00  1.0000E+00"
  ), path)
}

## Build a minimal .ext file
.write_minimal_ext <- function(path) {
  writeLines(c(
    "TABLE NO.  1: FINAL PARAMETER ESTIMATE",
    " ITERATION    THETA1       THETA2       OMEGA(1,1)  SIGMA(1,1)   OBJ",
    "  -1000000000  10.000       50.000       0.100        0.100       100.0"
  ), path)
}

## Set up a complete fake fit folder on disk
.make_fake_fit_folder <- function(dir) {
  data_path <- file.path(dir, "data.csv")
  .write_minimal_dataset(data_path)
  writeLines(.nm_model_code(data_path), file.path(dir, "run.mod"))
  .write_minimal_cov(file.path(dir, "run.cov"))
  .write_minimal_ext(file.path(dir, "run.ext"))
  invisible(dir)
}

test_that("run_sim_with_uncertainty: errors with clear message when run.cov is missing", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp
  .make_fake_fit_folder(tmp)
  unlink(file.path(tmp, "run.cov"))

  expect_error(
    run_sim_with_uncertainty(fit_folder = tmp, verbose = FALSE),
    regexp = "run\\.cov|covariance"
  )
})

test_that("run_sim_with_uncertainty: errors when fit_folder does not exist", {
  expect_error(
    run_sim_with_uncertainty(fit_folder = "/nonexistent/path/xyz"),
    class = "error"
  )
})

test_that("run_sim_with_uncertainty: errors when data argument is not a data.frame", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp
  .make_fake_fit_folder(tmp)

  # call_psn is never reached — data check fires first
  expect_error(
    run_sim_with_uncertainty(fit_folder = tmp, data = "not_a_dataframe", verbose = FALSE),
    regexp = "data.frame"
  )
})

test_that("run_sim_with_uncertainty (mocked): creates working folder and returns data.frame", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp

  fit_dir    <- file.path(tmp, "fit1")
  output_dir <- file.path(tmp, "output")
  dir.create(fit_dir);  dir.create(output_dir)
  .make_fake_fit_folder(fit_dir)

  ## Mock call_psn to create a fake vpc directory instead of running PsN
  local_mocked_bindings(
    call_psn = function(model_file, output_file, path, options, ...) {
      ## Extract the --dir= value from options
      dir_opt   <- options[grepl("^--dir=", options)]
      dir_name  <- sub("^--dir=", "", dir_opt)
      vpc_dir   <- file.path(path, dir_name)
      .make_fake_vpc_dir(vpc_dir, n = 5)
      invisible(NULL)
    },
    .package = "pharmr.extra"
  )

  out <- run_sim_with_uncertainty(
    fit_folder = fit_dir,
    n_samples  = 5,
    path       = output_dir,
    id         = "test_vpc_run",
    verbose    = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true("sample_id" %in% names(out))
  expect_equal(length(unique(out$sample_id)), 5)
})

test_that("run_sim_with_uncertainty (mocked): custom data replaces original dataset", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp

  fit_dir    <- file.path(tmp, "fit1")
  output_dir <- file.path(tmp, "output")
  dir.create(fit_dir);  dir.create(output_dir)
  .make_fake_fit_folder(fit_dir)

  custom_data <- data.frame(
    ID = 1:3, TIME = 0, DV = 0, AMT = 100,
    EVID = 1, MDV = 1
  )

  written_data_path <- NULL
  local_mocked_bindings(
    call_psn = function(model_file, output_file, path, options, ...) {
      dir_opt  <- options[grepl("^--dir=", options)]
      dir_name <- sub("^--dir=", "", dir_opt)
      vpc_dir  <- file.path(path, dir_name)
      .make_fake_vpc_dir(vpc_dir, n = 2)
      ## Capture the data.csv that was written to the vpc working folder
      written_data_path <<- file.path(path, "data.csv")
      invisible(NULL)
    },
    .package = "pharmr.extra"
  )

  run_sim_with_uncertainty(
    fit_folder = fit_dir,
    data       = custom_data,
    n_samples  = 2,
    path       = output_dir,
    id         = "test_custom_data",
    verbose    = FALSE
  )

  ## The vpc working folder's data.csv should contain our custom 3-subject data
  written_dat <- read.csv(written_data_path)
  expect_equal(nrow(written_dat), 3)
  expect_equal(length(unique(written_dat$ID)), 3)
})

test_that("run_sim_with_uncertainty (mocked): force=TRUE overwrites existing working folder", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp

  fit_dir    <- file.path(tmp, "fit1")
  output_dir <- file.path(tmp, "output")
  dir.create(fit_dir);  dir.create(output_dir)
  .make_fake_fit_folder(fit_dir)

  # Pre-create the working folder so force must remove it
  dir.create(file.path(output_dir, "force_test_run"))

  local_mocked_bindings(
    call_psn = function(model_file, output_file, path, options, ...) {
      dir_opt  <- options[grepl("^--dir=", options)]
      dir_name <- sub("^--dir=", "", dir_opt)
      .make_fake_vpc_dir(file.path(path, dir_name), n = 2)
    },
    .package = "pharmr.extra"
  )

  expect_no_error(
    run_sim_with_uncertainty(
      fit_folder = fit_dir,
      n_samples  = 2,
      path       = output_dir,
      id         = "force_test_run",
      force      = TRUE,
      verbose    = FALSE
    )
  )
})

test_that("run_sim_with_uncertainty (mocked): errors when working folder exists and force=FALSE", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  withr::local_tempdir() -> tmp

  fit_dir    <- file.path(tmp, "fit1")
  output_dir <- file.path(tmp, "output")
  dir.create(fit_dir);  dir.create(output_dir)
  .make_fake_fit_folder(fit_dir)
  dir.create(file.path(output_dir, "no_force_run"))

  expect_error(
    run_sim_with_uncertainty(
      fit_folder = fit_dir,
      n_samples  = 2,
      path       = output_dir,
      id         = "no_force_run",
      force      = FALSE,
      verbose    = FALSE
    ),
    regexp = "already exists|force = TRUE"
  )
})
