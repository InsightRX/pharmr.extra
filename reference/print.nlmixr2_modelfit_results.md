# Print function for an nlmixr2 fit shaped like a pharmpy modelfit

Reuses the same layout as
[print.pharmpy.workflows.results.ModelfitResults](https://insightrx.github.io/pharmr.extra/reference/print.pharmpy.workflows.results.ModelfitResults.md)
so NONMEM and nlmixr2 fits look identical at the console.

## Usage

``` r
# S3 method for class 'nlmixr2_modelfit_results'
print(x, ...)
```

## Arguments

- x:

  nlmixr2 fit object (pharmpy-shaped list produced by
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md))

- ...:

  ignored
