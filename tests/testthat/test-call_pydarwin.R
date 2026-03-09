library(mockery)

# Helpers ------------------------------------------------------------------

make_template_file <- function(dir) {
  path <- file.path(dir, "template.txt")
  writeLines(
    c(
      "$PROBLEM  THEOPP COMBINED",
      "$INPUT ID AMT WGT APGR DV TIME",
      "$DATA THEOPP.csv IGNORE=@",
      "$SUBROUTINES ADVAN1 TRANS2",
      "$PK",
      "  CL = THETA(1)*EXP(ETA(1)){ETAVCL[1]}",
      "  V  = THETA(2)*EXP(ETA(2)){ETAVV[1]}",
      "  S1 = V",
      "$ERROR",
      "  Y = F + F*EPS(1)",
      "$THETA (0, 0.1)  ; POP_CL",
      "$THETA (0, 0.5)  ; POP_V",
      "$OMEGA 0.04 ; IIV_CL{BLOCKVC[1]}",
      "$OMEGA 0.04 ; IIV_V",
      "$SIGMA 0.01",
      "$EST METHOD=1 INTER MAXEVAL=9999",
      "$COV UNCOND"
    ),
    path
  )
  path
}

make_tokens_file <- function(dir) {
  path <- file.path(dir, "tokens.json")
  writeLines(
    jsonlite::toJSON(
      list(
        ETAVCL = list("", "*EXP(ETA(3))"),
        ETAVV  = list("", "*EXP(ETA(4))"),
        BLOCKVC = list("", "\n$OMEGA BLOCK(2) 0.04 0.001 0.04")
      ),
      auto_unbox = FALSE
    ),
    path
  )
  path
}

make_options_file <- function(dir, extra = list()) {
  path <- file.path(dir, "options.json")
  opts <- utils::modifyList(
    list(
      algorithm       = "EX",
      nmfe_path       = "/usr/local/nm/run/nmfe75",
      num_parallel    = 2L,
      working_dir     = dir,
      output_dir      = dir
    ),
    extra
  )
  jsonlite::write_json(opts, path, auto_unbox = TRUE, pretty = TRUE)
  path
}

make_results_csv <- function(dir) {
  path <- file.path(dir, "results.csv")
  readr::write_csv(
    data.frame(
      model   = c("run1", "run2", "run3"),
      fitness = c(120.5, 115.2, 130.1),
      ofv     = c(120.5, 115.2, 130.1),
      nparams = c(5L, 6L, 5L)
    ),
    path
  )
  path
}


# pydarwin_options() -------------------------------------------------------

test_that("pydarwin_options returns a list with expected keys", {
  opts <- pydarwin_options()
  expect_type(opts, "list")
  expect_true(all(c("algorithm", "num_parallel", "num_generations",
                    "population_size", "random_seed", "model_run_timeout") %in% names(opts)))
})

test_that("pydarwin_options uses GA as default algorithm", {
  expect_equal(pydarwin_options()$algorithm, "GA")
})

test_that("pydarwin_options respects supplied arguments", {
  opts <- pydarwin_options(algorithm = "PSO", num_parallel = 8L, population_size = 100L)
  expect_equal(opts$algorithm, "PSO")
  expect_equal(opts$num_parallel, 8L)
  expect_equal(opts$population_size, 100L)
})

test_that("pydarwin_options passes through extra arguments via ...", {
  opts <- pydarwin_options(crossover_rate = 0.9, mutation_rate = 0.05)
  expect_equal(opts$crossover_rate, 0.9)
  expect_equal(opts$mutation_rate, 0.05)
})

test_that("pydarwin_options omits NULL fields for nmfe_path, working_dir, output_dir, project_name", {
  opts <- pydarwin_options()
  expect_false("nmfe_path"    %in% names(opts))
  expect_false("working_dir"  %in% names(opts))
  expect_false("output_dir"   %in% names(opts))
  expect_false("project_name" %in% names(opts))
})

test_that("pydarwin_options includes optional fields when set", {
  opts <- pydarwin_options(
    nmfe_path    = "/usr/local/nm/run/nmfe75",
    working_dir  = "/tmp/run",
    output_dir   = "/tmp/run/output",
    project_name = "MySearch"
  )
  expect_equal(opts$nmfe_path,    "/usr/local/nm/run/nmfe75")
  expect_equal(opts$working_dir,  "/tmp/run")
  expect_equal(opts$output_dir,   "/tmp/run/output")
  expect_equal(opts$project_name, "MySearch")
})


# read_pydarwin_results() --------------------------------------------------

test_that("read_pydarwin_results returns NULL when results.csv is absent", {
  dir <- withr::local_tempdir()
  expect_null(suppressMessages(read_pydarwin_results(dir, verbose = FALSE)))
})

test_that("read_pydarwin_results parses results.csv and sorts by fitness", {
  dir <- withr::local_tempdir()
  make_results_csv(dir)
  res <- read_pydarwin_results(dir, verbose = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  # should be sorted ascending by fitness
  expect_true(res$fitness[1] < res$fitness[nrow(res)])
})

test_that("read_pydarwin_results emits a warning when file is missing and verbose = TRUE", {
  dir <- withr::local_tempdir()
  expect_message(read_pydarwin_results(dir, verbose = TRUE), regexp = "No results.csv")
})


# call_pydarwin() input validation -----------------------------------------

test_that("call_pydarwin errors when template_file does not exist", {
  dir <- withr::local_tempdir()
  expect_error(
    call_pydarwin(
      template_file = file.path(dir, "missing_template.txt"),
      tokens_file   = file.path(dir, "tokens.json"),
      options       = pydarwin_options()
    ),
    regexp = "Template file not found"
  )
})

test_that("call_pydarwin errors when tokens_file does not exist", {
  dir <- withr::local_tempdir()
  tmpl <- make_template_file(dir)
  expect_error(
    call_pydarwin(
      template_file = tmpl,
      tokens_file   = file.path(dir, "missing_tokens.json"),
      options       = pydarwin_options()
    ),
    regexp = "Tokens file not found"
  )
})

test_that("call_pydarwin errors when options_file path is given but does not exist", {
  dir <- withr::local_tempdir()
  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)
  expect_error(
    call_pydarwin(
      template_file = tmpl,
      tokens_file   = tok,
      options_file  = file.path(dir, "missing_options.json")
    ),
    regexp = "Options file not found"
  )
})

test_that("call_pydarwin errors when neither options_file nor options are given", {
  dir <- withr::local_tempdir()
  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)
  expect_error(
    call_pydarwin(
      template_file = tmpl,
      tokens_file   = tok
    ),
    regexp = "options_file.*options"
  )
})


# call_pydarwin() mocked execution -----------------------------------------

test_that("call_pydarwin calls darwin$run_search with correct arguments", {
  local_pharmr.extra_options()
  dir <- withr::local_tempdir()

  tmpl  <- make_template_file(dir)
  tok   <- make_tokens_file(dir)
  make_results_csv(dir)

  run_search_mock <- mockery::mock(NULL)
  darwin_mock <- list(run_search = run_search_mock)

  stub(call_pydarwin, "get_nmfe_location",         function(...) "/usr/local/nm/run/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) TRUE)
  stub(call_pydarwin, "reticulate::import",         function(...) darwin_mock)

  opts <- pydarwin_options(
    algorithm  = "EX",
    output_dir = dir
  )

  result <- call_pydarwin(
    template_file = tmpl,
    tokens_file   = tok,
    options       = opts,
    folder        = dir,
    verbose       = FALSE
  )

  mockery::expect_called(run_search_mock, 1)
  call_args <- mockery::mock_args(run_search_mock)[[1]]
  expect_equal(call_args[[1]], normalizePath(tmpl, mustWork = TRUE))
  expect_equal(call_args[[2]], normalizePath(tok,  mustWork = TRUE))
  # third arg is the temp options JSON path (file is cleaned up on.exit, so check extension)
  expect_true(grepl("\\.json$", call_args[[3]]))
})

test_that("call_pydarwin injects nmfe_path from get_nmfe_location into options", {
  local_pharmr.extra_options()
  dir <- withr::local_tempdir()

  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)
  make_results_csv(dir)

  run_search_mock <- mockery::mock(NULL)
  darwin_mock     <- list(run_search = run_search_mock)
  written_opts    <- list()

  stub(call_pydarwin, "get_nmfe_location",              function(...) "/auto/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) TRUE)
  stub(call_pydarwin, "reticulate::import",              function(...) darwin_mock)
  stub(call_pydarwin, "jsonlite::write_json", function(x, path, ...) {
    written_opts <<- x
    jsonlite::write_json(x, path, ...)
  })

  call_pydarwin(
    template_file = tmpl,
    tokens_file   = tok,
    options       = pydarwin_options(algorithm = "EX", output_dir = dir),
    folder        = dir,
    verbose       = FALSE
  )

  expect_equal(written_opts$nmfe_path, "/auto/nmfe75")
})

test_that("call_pydarwin: options R list overrides values from options_file", {
  local_pharmr.extra_options()
  dir <- withr::local_tempdir()

  tmpl     <- make_template_file(dir)
  tok      <- make_tokens_file(dir)
  opts_f   <- make_options_file(dir, list(algorithm = "GA", num_parallel = 2L))
  make_results_csv(dir)

  written_opts <- list()
  darwin_mock  <- list(run_search = mockery::mock(NULL))

  stub(call_pydarwin, "get_nmfe_location",               function(...) "/usr/local/nm/run/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) TRUE)
  stub(call_pydarwin, "reticulate::import",              function(...) darwin_mock)
  stub(call_pydarwin, "jsonlite::write_json", function(x, path, ...) {
    written_opts <<- x
    jsonlite::write_json(x, path, ...)
  })

  call_pydarwin(
    template_file = tmpl,
    tokens_file   = tok,
    options_file  = opts_f,
    options       = list(algorithm = "EX", num_parallel = 8L),
    folder        = dir,
    verbose       = FALSE
  )

  # R list value takes precedence over options_file value
  expect_equal(written_opts$algorithm,    "EX")
  expect_equal(written_opts$num_parallel, 8L)
})

test_that("call_pydarwin returns output_dir and parsed results", {
  local_pharmr.extra_options()
  dir <- withr::local_tempdir()

  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)
  make_results_csv(dir)

  darwin_mock <- list(run_search = mockery::mock(NULL))

  stub(call_pydarwin, "get_nmfe_location",               function(...) "/usr/local/nm/run/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) TRUE)
  stub(call_pydarwin, "reticulate::import",              function(...) darwin_mock)

  result <- call_pydarwin(
    template_file = tmpl,
    tokens_file   = tok,
    options       = pydarwin_options(algorithm = "EX", output_dir = dir),
    folder        = dir,
    verbose       = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("output_dir", "results"))
  expect_equal(result$output_dir, dir)
  expect_s3_class(result$results, "data.frame")
  expect_equal(nrow(result$results), 3)
})

test_that("call_pydarwin errors clearly when darwin module is not available", {
  dir  <- withr::local_tempdir()
  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)

  stub(call_pydarwin, "get_nmfe_location",               function(...) "/usr/local/nm/run/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) FALSE)

  expect_error(
    call_pydarwin(
      template_file = tmpl,
      tokens_file   = tok,
      options       = pydarwin_options(algorithm = "EX"),
      verbose       = FALSE
    ),
    regexp = "darwin.*Python module|install_pydarwin"
  )
})

test_that("call_pydarwin propagates errors from darwin$run_search", {
  local_pharmr.extra_options()
  dir  <- withr::local_tempdir()
  tmpl <- make_template_file(dir)
  tok  <- make_tokens_file(dir)

  darwin_mock <- list(run_search = mockery::mock(stop("NONMEM not found")))

  stub(call_pydarwin, "get_nmfe_location",               function(...) "/usr/local/nm/run/nmfe75")
  stub(call_pydarwin, "reticulate::py_module_available", function(...) TRUE)
  stub(call_pydarwin, "reticulate::import",              function(...) darwin_mock)

  expect_error(
    call_pydarwin(
      template_file = tmpl,
      tokens_file   = tok,
      options       = pydarwin_options(algorithm = "EX"),
      folder        = dir,
      verbose       = FALSE
    ),
    regexp = "pyDarwin error"
  )
})
