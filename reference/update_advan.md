# Update the ADVAN number

Note: this only updates the ADVAN number in \$SUBROUTINEs, but does not
change anything in the remaining model code! The primary use case for
this function is for easy switching between ADVAN 6, 9, and 13 for ODE
models.

## Usage

``` r
update_advan(model, advan)
```

## Arguments

- model:

  a Pharmpy NONMEM model object

- advan:

  new advan
