# Call nmfe

Call nmfe

## Usage

``` r
call_nmfe(
  model_file,
  output_file,
  path,
  nmfe = "/opt/NONMEM/nm_cxurrent/run/nmfe75",
  console = FALSE,
  check_only = FALSE,
  verbose = FALSE,
  parafile = NULL,
  threads = NULL
)
```

## Arguments

- model_file:

  model file, e.g. "run.mod"

- output_file:

  output file, e.g. "run.lst"

- path:

  run folder path, e.g. "run1"

- nmfe:

  path to nmfe batch file to run NONMEM

- console:

  show output from nmfe in console? Default `FALSE`

- check_only:

  only run NM-TRAN, to check the model syntax

- verbose:

  verbose output?

- parafile:

  absolute path to a NONMEM parafile (MPI or FPI). If supplied, will be
  passed to nmfe as `-parafile=<path>`. Default `NULL` (no parafile).

- threads:

  number of nodes to request, passed to nmfe as `[nodes]=N`. Only
  applied when `parafile` is supplied.
