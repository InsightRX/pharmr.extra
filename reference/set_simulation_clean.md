# Set model to be a simulation (only) model

This function is a drop-in replacement of the Pharmpy `set_simulation()`
function. The Pharmpy function works fine in many instances, but in some
cases it modifies the variable declarations (e.g. redeclares variables
in \$PK that are declared in \$DES, which can lead to invalid models
that are not accepted by NONMEM).

## Usage

``` r
set_simulation_clean(model, seed, n, true_prior = FALSE)
```

## Arguments

- model:

  Pharmpy NONMEM model object

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

a Pharmpy NONMEM model object when `true_prior = FALSE`.

When `true_prior = TRUE` the return value is instead the NONMEM model
**code** (a single string). Pharmpy's `$SIMULATION` grammar does not
accept the `TRUE=PRIOR` option and refuses to parse such a control
stream, so the record can only be carried at the code level; the caller
is expected to write the string out itself rather than round-trip it
through Pharmpy.

## Details

This function just removes the \$ESTIMATION steps and adds the \$SIM
record, and does not modify any of the other NONMEM code.
