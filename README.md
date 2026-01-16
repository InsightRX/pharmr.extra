
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pharmr.extra

<!-- badges: start -->

[![R-CMD-check](https://github.com/InsightRX/pharmr.extra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InsightRX/pharmr.extra/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/InsightRX/pharmr.extra/graph/badge.svg)](https://app.codecov.io/gh/InsightRX/pharmr.extra)
<!-- badges: end -->

`pharmar.extra` is an R package that acts as an extension to `pharmr` (R
wrapper for Pharmpy). Pharmpy is a solid toolkit for automated model
development. It is developed at Uppsala University.

The goal of `pharmar.extra` is:

- to provide some functions that are (currently) still missing in
  pharmr. If they get added in the future in Pharmpy natively, they will
  be removed from this package.
- to provide stable and more user-friendly wrapper functions for
  performing certain tasks, such as running a model (`run_nlme()`) or
  exporting Pharmpy models and results.

## Installation

You can install the development version of pharmr.extra from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("InsightRX/pharmr.extra")
```

## Documentation

See at
[`https://insightrx.github.io/pharmr.extra/`](https://insightrx.github.io/pharmr.extra/reference/index.html)
and also in the installed package: `help(package = "pharmr.extra")`.

## License

MIT © InsightRX
