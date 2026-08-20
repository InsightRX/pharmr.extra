# Set the `$SIMULATION` record of a NONMEM control stream

Works on the model code rather than on a Pharmpy model object, because
Pharmpy cannot parse `TRUE=PRIOR` (see
[`set_simulation_clean()`](https://insightrx.github.io/pharmr.extra/reference/set_simulation_clean.md))
and because the `uncertainty_engine = "nwpri"` path of
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
only varies the seed and the subproblem count between chunks — a string
edit on an otherwise finished control stream.

## Usage

``` r
set_simulation_record(code, seed, n, true_prior = FALSE)
```

## Arguments

- code:

  NONMEM model code (single string or character vector of lines).

- seed:

  random seed number

- n:

  number of simulation subproblems to run

- true_prior:

  emit `TRUE=PRIOR`, i.e. have NONMEM draw a new parameter vector from
  the model's `$PRIOR` record for every subproblem (see
  [`add_nwpri_prior()`](https://insightrx.github.io/pharmr.extra/reference/add_nwpri_prior.md)).
  Default `FALSE`.

## Value

NONMEM model code as a single string.

## Details

An existing `$SIMULATION` record is replaced in place, so its position
relative to the `$TABLE` records is preserved. When the model has none,
the record is appended.
