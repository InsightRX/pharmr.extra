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

## Minimal fake run_nlme result
.mock_nlme_result <- function(tab = NULL) {
  if (is.null(tab)) {
    tab <- data.frame(
      ID   = c(1L, 1L, 1L),
      TIME = c(0, 6, 12),
      DV   = c(0, 5.1, 3.2),
      EVID = c(1L, 0L, 0L),
      PRED = c(0, 5.0, 3.0),
      CL   = c(2, 2, 2)
    )
  }
  result <- list()
  attr(result, "tables") <- list(simtab = tab)
  result
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
