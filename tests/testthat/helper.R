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
