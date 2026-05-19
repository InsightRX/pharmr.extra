# Update parameter estimates (and fix)

For example for using model in simulations.

## Usage

``` r
update_parameters(model, fit, fix = FALSE, verbose = FALSE)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- fit:

  pharmpy fit object

- fix:

  fix the estimates?

- verbose:

  verbose output?

## Value

the input pharmpy model with parameter estimates updated, or
`invisible(NULL)` if no estimates were available

## Details

Supports both NONMEM-backend and nlmixr-backend pharmpy models. The
`fit` argument may be:

- a pharmpy `ModelfitResults` object (NONMEM path)

- the pharmpy-shaped wrapper returned by
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  on an nlmixr model

- a raw `nlmixr2FitCore` object (output of `nlmixr2::nlmixr2()` called
  directly, without going through
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md))

For nlmixr2 fits, both diagonal and off-diagonal omega elements are
extracted so that block-omega parameters (named `IIV_X_IIV_Y` in
pharmpy) are updated alongside the variance terms.
