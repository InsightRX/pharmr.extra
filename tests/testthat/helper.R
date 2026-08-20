# Test fixtures: https://testthat.r-lib.org/articles/test-fixtures.html -------

# Call this function in tests to locally set options for maximal reproducibility.
#
# This is intentionally a no-op. We previously set cli.default_handler,
# rlib_message_verbosity, and rlib_warning_verbosity here to silence cli output
# in tests, but all three options prevent cli from *signaling* message/warning
# conditions. When they leak between tests via a C-level longjmp from reticulate
# (which bypasses R's on.exit() stack), all subsequent expect_message() /
# expect_warning() calls fail silently. Keeping this function as a no-op
# avoids the leakage while keeping call sites unchanged.
local_pharmr.extra_options <- function(..., .local_envir = parent.frame()) {
  invisible(NULL)
}

skip_if_nonmem_not_available <- function() {
  tryCatch(
    pharmr.extra::get_pharmpy_conf(),
    error = function(e) testthat::skip("NONMEM/Pharmpy not configured")
  )
}

## Skip unless a runnable NONMEM (nmfe) is reachable. Returns its path, so
## callers can `nmfe <- skip_if_nmfe_not_available()`.
skip_if_nmfe_not_available <- function() {
  nmfe <- tryCatch(get_nmfe_location(), error = function(e) NULL)
  if (is.null(nmfe) || !file.exists(nmfe)) {
    testthat::skip("NONMEM (nmfe) not available")
  }
  nmfe
}

make_model_with_cov <- function() {
  pharmr::read_model_from_string(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n$ERROR\nY=F+EPS(1)\n$THETA (0,10) ; POP_CL\n$THETA (0,50) ; POP_V\n$SIGMA 0.1\n$EST METHOD=1\n$COV UNCOND\n"
  )
}

make_model_without_cov <- function() {
  pharmr::read_model_from_string(
    "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV\n$DATA data.csv IGNORE=@\n$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)\nV=THETA(2)\nS1=V\n$ERROR\nY=F+EPS(1)\n$THETA (0,10) ; POP_CL\n$THETA (0,50) ; POP_V\n$SIGMA 0.1\n$EST METHOD=1\n"
  )
}

## Minimal fake simulation output table, as a run folder would yield it
.mock_sim_tab <- function() {
  data.frame(
    ID   = c(1L, 1L, 1L),
    TIME = c(0, 6, 12),
    DV   = c(0, 5.1, 3.2),
    EVID = c(1L, 0L, 0L),
    PRED = c(0, 5.0, 3.0),
    CL   = c(2, 2, 2)
  )
}

## Minimal fake run_nlme result
.mock_nlme_result <- function(tab = NULL) {
  if (is.null(tab)) {
    tab <- .mock_sim_tab()
  }
  result <- list()
  attr(result, "tables") <- list(simtab = tab)
  result
}

## Stand in for the execute half of the NONMEM `"replicates"` uncertainty
## engine (#129). `run_sim()` prepares every replicate's run folder for real --
## that half is Pharmpy work in the parent -- and this replaces the NONMEM run
## and table read a worker would do, so the tests need no NONMEM installation.
## `get_nmfe_location()` goes with it: it is resolved in the parent before any
## replicate is dispatched.
##
## `fn` takes the same arguments as `run_nonmem_sim_folder()` and returns one
## regimen's simulation table.
local_mock_nonmem_sim <- function(fn = NULL, .local_envir = parent.frame()) {
  if (is.null(fn)) {
    fn <- function(spec, nmfe, table_names, clean = TRUE) .mock_sim_tab()
  }
  testthat::local_mocked_bindings(
    get_nmfe_location = function(...) "/nonexistent/nmfe",
    run_nonmem_sim_folder = fn,
    .package = "pharmr.extra",
    .env = .local_envir
  )
}

## Minimal dataset that satisfies run_sim() column expectations
.sim_dat <- function(n_ids = 1) {
  lapply(seq_len(n_ids), function(i) {
    data.frame(
      ID   = i,
      TIME = c(0, 6, 12),
      DV   = c(0, 5, 3),
      AMT  = c(100, 0, 0),
      EVID = c(1, 0, 0),
      MDV  = c(1, 0, 0)
    )
  }) |> dplyr::bind_rows()
}


## Simulation output as run_sim_nlmixr() returns it, for tests that mock the
## nlmixr2 engine out.
.mock_sim_table <- function() {
  data.frame(
    ID            = c(1L, 1L, 1L),
    TIME          = c(0, 6, 12),
    DV            = c(0, 5.1, 3.2),
    IPRED         = c(0, 5.1, 3.2),
    PRED          = c(0, 5.0, 3.0),
    EVID          = c(1L, 0L, 0L),
    regimen_label = "original regimens"
  )
}


## ---------------------------------------------------------------------------
## cli status-bar helpers (issue #137)
##
## cli implements progress bars on top of its status-bar stack, so a
## `cli_process_done()` that has no matching `cli_process_start()` silently
## closes whatever *is* on that stack -- including a caller's progress bar.
## cli then indexes the emptied stack and throws "subscript out of bounds" on
## the bar's next update or on its teardown, after the work has finished.
## These helpers let tests assert the stack is left as it was found.
## ---------------------------------------------------------------------------

## Number of open cli status bars. Uses cli internals, so skip rather than fail
## if they move.
.cli_status_depth <- function() {
  app <- tryCatch(cli:::default_app(), error = function(e) NULL)
  if(is.null(app)) testthat::skip("cli status-bar internals not available")
  length(app$status_bar)
}

## Open a status bar to stand in for a caller's progress bar, and close it when
## the calling test finishes. Returns the depth of the stack while it is open.
local_cli_outer_status <- function(.local_envir = parent.frame()) {
  id <- cli::cli_process_start("outer", .auto_close = FALSE)
  withr::defer(
    suppressWarnings(try(cli::cli_process_done(id = id), silent = TRUE)),
    envir = .local_envir
  )
  .cli_status_depth()
}

## Make cli render progress bars immediately and on every update. The failure
## in issue #137 only happens once a bar has actually been shown, and cli only
## shows one after `cli.progress_show_after` seconds *and* on a timer tick, so
## both have to be forced for a test to reach that code path.
local_cli_progress_forced <- function(.local_envir = parent.frame()) {
  ## Both internals are resolved defensively: if a future cli renames either,
  ## the test has to skip rather than error out.
  clienv <- tryCatch(cli:::clienv, error = function(e) NULL)
  tick_set <- tryCatch(cli:::cli_tick_set, error = function(e) NULL)
  if(!is.environment(clienv) || !is.function(tick_set)) {
    testthat::skip("cli progress internals not available")
  }
  withr::local_options(
    list(cli.progress_show_after = 0, cli.dynamic = FALSE, cli.ansi = FALSE),
    .local_envir = .local_envir
  )
  old_tick <- clienv$tick_time
  tick_set(tick_time = 1)
  withr::defer(tick_set(tick_time = old_tick), envir = .local_envir)
  invisible(NULL)
}
