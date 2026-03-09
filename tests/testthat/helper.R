# Test fixtures: https://testthat.r-lib.org/articles/test-fixtures.html -------

# Call this function in tests to locally set options for maximal reproducibility.
# Mainly just used to silence cli for cleaner looking test() output.
#
# Note: we only suppress the cli output handler here, not rlib_message_verbosity
# or rlib_warning_verbosity. Those options prevent cli from *signaling*
# message/warning conditions, which breaks expect_message() / expect_warning()
# if they ever leak between tests (e.g. via a C-level longjmp from reticulate).
local_pharmr.extra_options <- function(
    cli.default_handler = function(...) { },
    .local_envir = parent.frame()
) {
  withr::local_options(
    # Suppresses all cli output, see:
    # https://github.com/r-lib/cli/issues/434#issuecomment-1064900549
    cli.default_handler = cli.default_handler,
    .local_envir = .local_envir
  )
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
