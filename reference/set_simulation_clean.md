# Set model to be a simulation (only) model

This function is a drop-in replacement of the Pharmpy `set_simulation()`
function. The pharmpy function works fine in many instances, but in some
cases it modifies the variable declarations (e.g. redeclares variables
in \$PK that are declared in \$DES, which can lead to invalid models
that are not accepted by NONMEM).

## Usage

``` r
set_simulation_clean(model, seed, n)
```

## Arguments

- model:

  Pharmpy NONMEM model object

- seed:

  random seed number

- n:

  number of simulation subproblems to run

## Value

a Pharmpy NONMEM model object

## Details

This function just removes the \$ESTIMATION steps and adds the \$SIM
record, and does not modify any of the other NONMEM code.
