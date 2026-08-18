# Attach fit info and tables to a fit object, e.g. from model fit or Pharmpy grid search final results

Attach fit info and tables to a fit object, e.g. from model fit or
Pharmpy grid search final results

## Usage

``` r
attach_fit_info(
  fit,
  model,
  fit_folder,
  output_file = "run.lst",
  is_sim_model = FALSE,
  verbose = TRUE
)
```

## Arguments

- fit:

  pharmpy fit object

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- fit_folder:

  Folder the run was executed in, holding the output tables and the
  estimation output file.

- output_file:

  NONMEM output file, default is `run.lst`

- is_sim_model:

  Is `fit` the result of a simulation rather than an estimation?
  Simulation results have no fit summary and no residuals to repair.

- verbose:

  verbose output?

## Value

The fit object with the model, the output tables and (for estimation
runs) a fit summary attached as the `model`, `tables` and `info`
attributes, and with `residuals` rebuilt into a joinable frame (see the
`Value` section of
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)).

Note that for a Pharmpy fit this is a *new* object — `ModelfitResults`
is a frozen dataclass, so replacing `residuals` means replacing the
object. Attributes already set on the fit passed in are carried over,
but any reference the caller still holds points at the un-repaired fit.
