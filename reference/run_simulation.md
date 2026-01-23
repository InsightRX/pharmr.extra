# Run simulation from a fitted object

Using fit object from fit with nlmixr2.

## Usage

``` r
run_simulation(
  obj,
  dose,
  interval,
  n_doses = NULL,
  n_days = 5,
  n_subjects = 500,
  aggregate = TRUE,
  bsv = TRUE,
  res_error = TRUE,
  group = NULL,
  path = NULL,
  ...
)
```

## Arguments

- obj:

  fit object from nlmixr2

- dose:

  vector of dose amounts. Needs to match length of `interval`

- interval:

  vector of dosing intervals. Needs to match length of `dose`

- n_doses:

  number of doses to simulate

- n_days:

  alternative to `n_doses`, specify number of days. `n_days` takes
  precedence if both are specified.

- n_subjects:

  number of subjects to simulate

- aggregate:

  summarize the data using mean, median, sd, etc.

- bsv:

  Simulate using between subject variability

- res_error:

  Add residual unexplained error to the simulated data?

- group:

  grouping to be added to aggregated data?

- path:

  path to file to store output object from fit.

- ...:

  arguments passed on to rxode2::rxSolve() function

## Value

TODO
