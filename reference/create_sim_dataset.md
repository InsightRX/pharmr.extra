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
  `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`. An
  optional `per` element names a covariate column in the dataset; each
  subject's dose is then multiplied by their value of that column,
  enabling weight- or BSA-based dosing. E.g.
  `regimen = list(dose = 5, per = "WT", interval = 24, n = 5, route = "sc")`
  gives a 5 mg/kg dose using the `WT` column. Alternatively, regimens
  can be specified as a data.frame. The data.frame specifies all dosing
  times (`dose`, `time` columns) and `route` and `t_inf` / `rate`. The
  data.frame may also optionally contain a `regimen` column that
  specifies a name for the regimen. This can be used to simulate
  multiple regimens. A function may also be supplied. It will be called
  once per subject with a data.frame of that subject's rows, and must
  return a named list accepted by
  [`create_regimen()`](https://insightrx.github.io/pharmr.extra/reference/create_regimen.md)
  (`dose`, `interval`, `n`, `route`; optionally `t_inf`, `per`,
  `regimen`). This enables fully custom per-subject dosing logic such as
  tiered weight-band dosing.

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

## Examples

``` r
if (FALSE) { # \dontrun{
model <- pharmr::read_model("run1.mod")

# Basic: use the model's original dataset with custom observation times
sim_dat <- create_sim_dataset(
  model = model,
  t_obs = seq(0, 168, by = 4)
)

# Replace regimen with a flat 500 mg oral dose every 12 h for 5 doses
sim_dat <- create_sim_dataset(
  model  = model,
  regimen = list(dose = 500, interval = 12, n = 5, route = "oral"),
  t_obs  = seq(0, 72, by = 2)
)

# Weight-based dosing (5 mg/kg) using the `per` element —
# requires a WT column in the dataset
sim_dat <- create_sim_dataset(
  model   = model,
  regimen = list(dose = 5, per = "WT", interval = 24, n = 3, route = "sc"),
  t_obs   = seq(0, 72, by = 4)
)

# Tiered weight-band dosing via a function
dose_fn <- function(x) {
  dose <- if (x$WT[1] < 40) 100 else if (x$WT[1] < 80) 200 else 250
  list(dose = dose, interval = 14 * 24, route = "sc", n = 6)
}
sim_dat <- create_sim_dataset(
  model   = model,
  regimen = dose_fn,
  t_obs   = seq(0, 84 * 24, by = 24)
)

# Simulate with sampled covariates from an external data.frame
covs <- data.frame(WT = c(55, 72, 88), AGE = c(34, 51, 67))
sim_dat <- create_sim_dataset(
  model      = model,
  covariates = covs,
  regimen    = list(dose = 500, interval = 12, n = 5, route = "oral"),
  t_obs      = seq(0, 72, by = 2)
)

# Simulate multiple regimens for comparison
regimens <- combine_regimens(
  "low"  = list(create_regimen(dose = 250, interval = 12, n = 5, route = "oral")),
  "high" = list(create_regimen(dose = 500, interval = 12, n = 5, route = "oral"))
)
sim_dat <- create_sim_dataset(
  model   = model,
  regimen = regimens,
  t_obs   = seq(0, 72, by = 2)
)
} # }
```
