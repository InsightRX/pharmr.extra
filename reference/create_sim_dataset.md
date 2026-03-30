# Create a NONMEM dataset for simulation

Prepares a dataset for use with
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md),
handling covariate sampling, regimen replacement, and observation record
creation. The returned data.frame can be passed directly to
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
as the `data` argument.

## Usage

``` r
create_sim_dataset(
  model,
  data = NULL,
  regimen = NULL,
  t_obs = NULL,
  covariates = NULL,
  n_subjects = NULL,
  input_from_data = FALSE,
  verbose = TRUE
)
```

## Arguments

- model:

  a Pharmpy model object, or a path to a NONMEM model file (`.mod`). If
  a file path is supplied, the model is loaded with
  [`pharmr::read_model()`](https://rdrr.io/pkg/pharmr/man/read_model.html)
  so that the `$DATA` path can be resolved.

- data:

  optional data.frame (or path to a CSV file) to use as the base dataset
  instead of the dataset attached to `model`. Useful when you want to
  apply `t_obs` or `regimen` changes to an already-prepared dataset. It
  is assumed that the column names in the dataset match the *order* of
  the columns in \$INPUT in the model. If this is not the case, the
  creation of the dataset may fail, or the simulations from the dataset
  may fail.

- regimen:

  if specified, will replace the regimens for each subject with a custom
  regimen. Can be specified in two ways. The simplest way is to just
  specify a list with elements `dose`, `interval`, `n`, and `route` (and
  `t_inf` / `rate` for infusions). E.g.
  `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
  Alternatively, regimens can be specified as a data.frame. The
  data.frame specifies all dosing times (`dose`, `time` columns) and
  `route` and `t_inf` / `rate`. The data.frame may also optionally
  contain a `regimen` column that specifies a name for the regimen. This
  can be used to simulate multiple regimens.

- t_obs:

  a vector of observation times. If specified, will override the
  observations in each subject in the input dataset.

- covariates:

  if specified, will replace subjects with subjects specified in a
  data.frame. In the data.frame, the column names should correspond
  exactly to any covariates included in the model. An `ID` column is
  optional; if absent, IDs are generated as `1:nrow(covariates)`. For
  time-varying covariates, a `TIME` column is also required (otherwise
  it will be assumed covariates are not changing over time).

- n_subjects:

  number of subjects to simulate, when using sampled data (i.e. requires
  `covariates` argument)

- verbose:

  logical; print progress messages.

## Value

data.frame with a NONMEM-format simulation dataset. A `.regimen` column
is included and is used internally by
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
to loop over multiple dosing regimens.
