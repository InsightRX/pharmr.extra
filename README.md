<!-- badges: start -->
[![R-CMD-check](https://github.com/InsightRX/pharmr.extra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InsightRX/pharmr.extra/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# pharmr.extra

`pharmar.extra` is an R package that acts as an extension to `pharmr` (R wrapper
for Pharmpy). Pharmpy is a solid toolkit for automated model development. It is
developed at Uppsala University.

The goal of `pharmar.extra` is:
- to provide some functions that are (currently) still missing in pharmr. If they get added in the future in Pharmpy natively, they will be removed from this package.
- to provide stable and more user-friendly wrapper functions for performing
certain tasks, such as running a model (`run_nlme()`) or exporting Pharmpy 
models and results.

## Installation

`pharmr.extra` is currently not available on CRAN. It can be installed using:

```r
remotes::install_github("InsightRX/pharmr.extra")
```
