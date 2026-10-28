# Fit model using NONMEM

No longer supported: this relied on a `run_nmfe()` helper that is not
part of the package, so it could never complete. Use
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
to fit NONMEM models.

## Usage

``` r
fit_model_nonmem(model, data = NULL, ...)
```

## Arguments

- model:

  pharmpy model object

- data:

  data.frame with data to fit

- ...:

  passed onto the tool-specific fitting function
