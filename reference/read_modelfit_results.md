# Read NONMEM modelfit results, with bug fixes for SIR covariance models

Wraps
[`pharmr::read_modelfit_results()`](https://rdrr.io/pkg/pharmr/man/read_modelfit_results.html)
with patches for Pharmpy bugs that arise when a SIR covariance step is
used (`$COVARIANCE SIRSAMPLE=...`). NONMEM writes a second ext-file
table (method "Importance Sampling of Variance-Covariance (SIR)") that
Pharmpy mishandles in three ways:

## Usage

``` r
read_modelfit_results(path, ...)
```

## Arguments

- path:

  Path to NONMEM model file (e.g. `"run1/run.mod"`).

- ...:

  Additional arguments passed to
  [`pharmr::read_modelfit_results()`](https://rdrr.io/pkg/pharmr/man/read_modelfit_results.html).

## Value

A Pharmpy `ModelfitResults` object (same as
[`pharmr::read_modelfit_results()`](https://rdrr.io/pkg/pharmr/man/read_modelfit_results.html)).

## Details

**Bug 1** (`_parse_parameter_estimates` — NaN final estimates): The SIR
table contains ITERATION 0 (FOCE starting point) and iterations 1…N (SIR
samples). Because the SIR modal OFV at `-1000000000` differs from the
last sample OFV, `_get_iter_df` appends a NaN sentinel row. This NaN row
is the last row of the combined `pe` DataFrame, so
`pe.iloc[-1].isnull().all()` is True and `final_pe` is set to all-NaN
instead of the FOCE estimates.

**Bug 2** (`_parse_table_numbers` — wrong covstatus): Including the SIR
table in `table_numbers` makes `covstatus_table_number` point to the SIR
table (number 2). The SIR block in the lst-file has "Elapsed estimation
time" but no "Elapsed covariance time", so `covariance_step_ok` is
`None` and the `.cov`/`.cor` files are never read, even though the
standard covariance step ran successfully under table 1.

**Bug 3** (`_parse_standard_errors` — cov_abort with missing
-1000000005): If the SIR ext table has row `-1000000001` (SEs) but not
`-1000000005` (omega/sigma SEs in stdcorr form), the original code
overrides the valid SEs with NaN and sets `cov_abort = True`, blocking
the `.cov` file read.

The fixes skip the SIR table in `_parse_table_numbers`, `_parse_ofv`,
and `_parse_parameter_estimates` (so only the FOCE table is used for
estimates and covariance-step lookup), while `_parse_standard_errors`
still uses the last (SIR) table to extract SIR-based standard errors.
