# Run a simulation against an nlmixr-format model with rxode2

Internal companion to
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md);
called when the input model is a pharmpy nlmixr-backend model. Uses
[`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
directly so we can avoid the pharmpy-driven nlmixr fitting/simulation
path (which requires the Python `pyreadr` package).

## Usage

``` r
run_sim_nlmixr(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = irxutils::get_random_id("sim_"),
  path = NULL,
  n_iterations = 1,
  variables = NULL,
  add_pk_variables = FALSE,
  output_file = "simtab",
  seed = 12345,
  verbose = TRUE
)
```

## Arguments

- fit:

  a Pharmpy modelfit object.

- data:

  a NONMEM-format data.frame to use as the simulation dataset. Typically
  the output of
  [`create_sim_dataset()`](https://insightrx.github.io/pharmr.extra/reference/create_sim_dataset.md).
  If `NULL`, the dataset attached to `model` is used as-is.

- model:

  either a Pharmpy model object, or a filename (for a model with NONMEM
  model code). If the latter,
  [`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
  will attempt to load the model into Pharmpy first.

- id:

  base run id (default a random `sim_*`). Each regimen is run in its own
  subfolder `id/regimen_<i>` (`<i>` = 1-based regimen index), so
  regimens don't overwrite each other's output.

- path:

  ignored for the nlmixr2 backend: simulations run via
  [`rxode2::rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  and create no NONMEM-style run folders. Accepted only to keep the
  signature aligned with
  [`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md).

- n_iterations:

  number of iterations of the entire simulation to perform. The dataset
  for the simulation will stay the same between each iterations.

- variables:

  vector of variables to output. If `NULL`, will output default
  variables `c("ID", "TIME", "DV", "EVID", "PRED")` as well as all
  variables declared in the NONMEM code.

- add_pk_variables:

  calculate basic PK variables: CMAX_OBS, TMAX_OBS, CMIN_OBS, and (when
  `CL` is in the output table) AUC_SS. AUC_SS is derived as the last
  dose in the simulation dataset divided by CL.

- output_file:

  TODO

- seed:

  TODO

- verbose:

  verbose output?

## Details

Returns a data.frame in the same shape as the NONMEM-side simulation
output (`ID`, `TIME`, `DV`, `IPRED`, `PRED`, `EVID`, plus declared
variables and a `regimen_label` column), so downstream example code that
plots simulation results works unchanged.

Limitation: `PRED` is reported as `IPRED` (no separate population
prediction); rxSolve does not produce both in a single call.
