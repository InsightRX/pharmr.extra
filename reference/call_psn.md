# Call PsN

Call PsN

## Usage

``` r
call_psn(
  model_file,
  output_file,
  path,
  options = c(),
  tool = c("execute", "vpc", "bootstrap", "sir", "proseval", "update_inits", "cdd"),
  console = TRUE,
  verbose = TRUE,
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

- options:

  a vector of arguments to pass to the PsN tool, e.g.
  `c("--samples=100", "--dir="test")`

- tool:

  TODO

- console:

  show output from nmfe in console? Default `FALSE`

- verbose:

  verbose output?

- parafile:

  absolute path to a NONMEM parafile (MPI or FPI). If supplied, will be
  passed to PsN as `--parafile=<path>`. Default `NULL` (no parafile).

- threads:

  number of nodes to request, passed to PsN as `--nodes=N` (within-model
  parallelism). Only applied when `parafile` is supplied.

## Value

TODO
