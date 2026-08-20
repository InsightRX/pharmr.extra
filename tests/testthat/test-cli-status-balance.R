## Regression tests for issue #137.
##
## `cli` builds progress bars on top of its status-bar stack. A
## `cli_process_done()` with no `id` closes whichever status bar happens to be
## on top of that stack, so a function that calls it without having opened one
## closes its *caller's* bar. cli then indexes the emptied stack and throws
## `subscript out of bounds` from the bar's next update or from its deferred
## teardown -- i.e. after the real work has already succeeded.
##
## The invariant these tests pin down: a function must leave cli's status-bar
## stack exactly as deep as it found it.

test_that("call_nmfe (verbose = FALSE) leaves cli's status-bar stack untouched", {
  skip_on_os("windows")

  ## A stand-in for nmfe: `call_nmfe()` only requires the file to exist and to
  ## be runnable, so no NONMEM install is needed to exercise the cli path.
  fake_nmfe <- file.path(withr::local_tempdir(), "nmfe")
  writeLines(c("#!/bin/sh", "exit 0"), fake_nmfe)
  Sys.chmod(fake_nmfe, "0755")

  run_dir <- withr::local_tempdir()
  writeLines("$PROBLEM test", file.path(run_dir, "run.mod"))

  before <- local_cli_outer_status()
  expect_gt(before, 0)

  call_nmfe(
    model_file = "run.mod",
    output_file = "run.lst",
    path = run_dir,
    nmfe = fake_nmfe,
    console = FALSE,
    verbose = FALSE
  )

  expect_equal(.cli_status_depth(), before)
})

test_that("call_nmfe (verbose = TRUE) closes its own status bar and no other", {
  skip_on_os("windows")

  fake_nmfe <- file.path(withr::local_tempdir(), "nmfe")
  writeLines(c("#!/bin/sh", "exit 0"), fake_nmfe)
  Sys.chmod(fake_nmfe, "0755")

  run_dir <- withr::local_tempdir()
  writeLines("$PROBLEM test", file.path(run_dir, "run.mod"))

  before <- local_cli_outer_status()

  call_nmfe(
    model_file = "run.mod",
    output_file = "run.lst",
    path = run_dir,
    nmfe = fake_nmfe,
    console = FALSE,
    verbose = TRUE
  )

  expect_equal(.cli_status_depth(), before)
})

test_that("call_nmfe (check_only) does not leak its status bar", {
  skip_on_os("windows")

  ## `check_only = TRUE` returns early; the status bar opened before that
  ## return has to be closed on the way out rather than left on the stack.
  bin_dir <- withr::local_tempdir()
  fake_nmfe <- file.path(bin_dir, "nmfe")
  writeLines(c("#!/bin/sh", "exit 0"), fake_nmfe)
  Sys.chmod(fake_nmfe, "0755")
  ## get_nmtran_from_nmfe() looks one folder up, then in tr/NMTRAN.exe
  tr_dir <- file.path(dirname(bin_dir), "tr")
  dir.create(tr_dir, showWarnings = FALSE, recursive = TRUE)
  fake_nmtran <- file.path(tr_dir, "NMTRAN.exe")
  writeLines(c("#!/bin/sh", "exit 0"), fake_nmtran)
  Sys.chmod(fake_nmtran, "0755")

  run_dir <- withr::local_tempdir()
  writeLines("$PROBLEM test", file.path(run_dir, "run.mod"))

  before <- local_cli_outer_status()

  res <- call_nmfe(
    model_file = "run.mod",
    output_file = "run.lst",
    path = run_dir,
    nmfe = fake_nmfe,
    console = FALSE,
    check_only = TRUE,
    verbose = TRUE
  )

  expect_true(as.logical(res))
  expect_equal(.cli_status_depth(), before)
})

test_that("attach_fit_info (sim model) leaves cli's status-bar stack untouched", {
  ## Sim models skip the "Summarizing fit results" status bar, so the close
  ## that pairs with it must be skipped too.
  local_mocked_bindings(
    get_tables_from_fit = function(...) list(simtab = data.frame(ID = 1L)),
    .package = "pharmr.extra"
  )

  before <- local_cli_outer_status()

  attach_fit_info(
    fit = list(),
    model = list(),
    fit_folder = withr::local_tempdir(),
    is_sim_model = TRUE,
    verbose = TRUE
  )

  expect_equal(.cli_status_depth(), before)
})

test_that("call_psn leaves cli's status-bar stack untouched", {
  skip_on_os("windows")

  run_dir <- withr::local_tempdir()
  writeLines("$PROBLEM test", file.path(run_dir, "run.mod"))

  before <- local_cli_outer_status()

  ## PsN is not installed in CI; the call aborts on exit code 127. Either way
  ## it must not have touched the caller's status bar on the way through.
  try(
    call_psn(
      model_file = "run.mod",
      output_file = "run.lst",
      path = run_dir,
      tool = "execute",
      console = FALSE,
      verbose = FALSE
    ),
    silent = TRUE
  )

  expect_equal(.cli_status_depth(), before)
})
