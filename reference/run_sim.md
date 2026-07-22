# Run simulations

Run simulations

## Usage

``` r
run_sim(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = irxutils::get_random_id("sim_"),
  force = FALSE,
  tool = c("auto", "nonmem", "nlmixr2"),
  n_iterations = 1,
  n_uncertainty = NULL,
  variables = NULL,
  add_pk_variables = FALSE,
  output_file = "simtab",
  update_table = TRUE,
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
  model code). If the latter, `run_sim()` will attempt to load the model
  into Pharmpy first.

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- tool:

  the tool to run the model in, either `nonmem`, or `nlmixr`.

- n_iterations:

  number of iterations of the entire simulation to perform. The dataset
  for the simulation will stay the same between each iterations.

- n_uncertainty:

  number of parameter sets to draw from the fit's covariance matrix to
  propagate parameter uncertainty. If `NULL` (default) or `0`, the point
  estimates are used and no uncertainty is propagated. If a positive
  integer, the point estimate is omitted and `n_uncertainty` parameter
  sets are sampled instead; one simulation is run per draw with its
  thetas/omegas/sigmas updated, so a total of
  `n_iterations * n_uncertainty` simulations are performed. Requires a
  `fit` object carrying a covariance matrix (i.e. the model was run with
  a `$COVARIANCE` step or SIR). When set, the output gains a
  `.uncertainty` column counting the replicate (1-based).

  Only parameters present in the covariance matrix are resampled; any
  other estimated parameters are held at their point estimates and a
  warning lists them. This matters for nlmixr2 fits in particular: the
  default nlmixr2 covariance step reports uncertainty only for the
  population fixed effects, so residual and random-effect variance
  parameters (SIGMA, OMEGA/IIV) are held fixed. For full uncertainty on
  those, use a bootstrap (`nlmixr2est::bootstrapFit()`). NONMEM
  `$COVARIANCE` typically covers all parameters, so all are resampled.

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

- update_table:

  should any existing \$TABLE records be removed, and a new `simtab` be
  created? This is default. If `FALSE`, it will leave \$TABLEs as
  specifed in the model. However, in the return object, only the first
  table is returned back. If `FALSE`, the `add_pk_variables` argument
  will be ignored.

- seed:

  TODO

- verbose:

  verbose output?

## Value

data.frame with simulation results
