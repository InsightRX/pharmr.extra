# Run a simulation based on supplied parameters estimates, and combine into proper format for VPC

This rewrite of `create_vpc_data()` removes the Pharmpy/pharmr
dependency from every step that runs before
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md).
The only Pharmpy touch is reading `model$code` as a string when a
Pharmpy model is supplied. All subsequent manipulation of the NONMEM
code (parameter updates, record removal/insertion, \$EST -\> \$SIM
conversion) and the input dataset (sanitisation of string-typed columns)
is done in pure R, so the Python/R \<-\> reticulate serialisation cannot
reject a value that NONMEM itself would accept.
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
is still used to actually invoke nmfe, and post-processing of
`obs`/`sim` is identical to the original.

## Usage

``` r
create_vpc_data(
  fit = NULL,
  model = NULL,
  data = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE,
  id = NULL,
  use_pharmpy = TRUE,
  fix_input_heuristic = TRUE,
  seed = NULL,
  id_format = "sF11.0"
)
```

## Arguments

- fit:

  fit object from
  [`pharmr::run_modelfit()`](https://rdrr.io/pkg/pharmr/man/run_modelfit.html).
  Optional; alternative to `model` + `parameters`.

- model:

  Either a Pharmpy model object (its `$code` is read), a character
  vector of NONMEM model code, or a path to a `.mod` file.

- data:

  Path to the NONMEM-ready CSV. Optional; if missing, the function reads
  `$DATA` from the model code. For nlmixr2 models a `data.frame` is
  accepted too, and the default is the dataset the model was fitted
  against.

- parameters:

  Named list of parameter inits, e.g.
  `list(THETA_1 = 0.23, OMEGA_1_1 = 0.097, SIGMA_1_1 = 1)`. Optional.

- keep_columns:

  character vector of column names in the original dataset to keep in
  the output.

- n:

  number of simulation iterations.

- verbose:

  verbose output?

- id:

  run id used as folder name. Defaults to a random name. NONMEM models
  only; ignored (with a warning) for nlmixr2 models, which run through
  [`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  and create no run folder.

- use_pharmpy:

  retained for backward compatibility; controls whether `PRED`/`TAD` are
  transferred from obs to sim during post-processing. NONMEM models
  only; ignored (with a warning) for nlmixr2 models.

- fix_input_heuristic:

  If TRUE (default), detect the common
  [`pharmr::set_dataset()`](https://rdrr.io/pkg/pharmr/man/set_dataset.html)
  side-effect that rewrites `$INPUT` to the CSV headers, and rebind
  `TIME` -\> `TAFD` and (for log-transform-both-sides models) `DV` -\>
  `LNDV`. Set to FALSE to leave `$INPUT` untouched. The LTBS detection
  only scans the `$ERROR` record (not the whole model) so a `LOG()` call
  in a covariate transform doesn't trigger a false swap. NONMEM models
  only; ignored (with a warning) for nlmixr2 models.

- seed:

  integer seed passed to the simulation step. Default `NULL` draws a
  random seed per call (so repeated `create_vpc_data()` calls in one
  session aren't pinned to identical draws); supply a value for
  bit-reproducible runs.

- id_format:

  NONMEM `$TABLE` `IDFORMAT` for the simulation table, which sets the
  output format of the `ID` column only. Defaults to `sF11.0`, so
  integer subject IDs of up to 10 digits are written in full instead of
  being truncated by NONMEM's default (~6 significant digits). All other
  columns keep NONMEM's default format. `NULL` uses the NONMEM default
  for `ID` too. NONMEM models only; ignored (with a warning) for nlmixr2
  models.

## Value

list with `obs` and `sim` data frames.

## nlmixr2 models

Pharmpy models in nlmixr format are routed to an rxode2-based path
([`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html))
instead of NONMEM. `fit`, `model`, `data`, `parameters`, `keep_columns`,
`n`, `seed` and `verbose` all apply there; the remaining arguments are
NONMEM-specific and warn if set. `sim$PRED` is a genuine population
prediction (a second solve with the between-subject random effects
zeroed), except on rxode2 versions without `zeroRe()`, where it falls
back to `IPRED`.
