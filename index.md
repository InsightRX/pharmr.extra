# pharmr.extra

`pharmar.extra` is an R package that acts as an extension to `pharmr` (R
wrapper for Pharmpy). Pharmpy is a solid toolkit for automated model
development. It is developed at Uppsala University.

The goal of `pharmar.extra` is:

- to provide some functions that are (currently) still missing in
  pharmr. If they get added in the future in Pharmpy natively, they will
  be removed from this package.
- to provide stable and more user-friendly wrapper functions for
  performing certain tasks, such as running a model
  ([`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md))
  or exporting Pharmpy models and results.

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
and also in the installed package:
[`help(package = "pharmr.extra")`](https://rdrr.io/pkg/pharmr.extra/man).

## License

MIT © InsightRX
