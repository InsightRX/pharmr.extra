# Test fixtures: https://testthat.r-lib.org/articles/test-fixtures.html -------

# Call this function in tests to locally set options for maximal reproducibility.
# Mainly just used to silence cli for cleaner looking test() output.
local_pharmr.extra_options <- function(
    cli.default_handler = function(...) { },
    rlib_message_verbosity = "quiet",
    rlib_warning_verbosity = "quiet",
    .local_envir = parent.frame()
) {
  withr::local_options(
    # Suppresses all cli output, see:
    # https://github.com/r-lib/cli/issues/434#issuecomment-1064900549
    cli.default_handler = cli.default_handler,
    rlib_message_verbosity = rlib_message_verbosity,
    rlib_warning_verbosity = rlib_warning_verbosity,
    .local_envir = .local_envir
  )
}
