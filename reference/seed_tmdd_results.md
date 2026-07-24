# Seed TMDD target-binding parameters into a base-fit results object

Returns a copy of `results` whose `parameter_estimates` gains `POP_KM`
(= `kd`) and `POP_CLMM` (= the fitted `POP_CL`). Pharmpy's TMDD
`structsearch` uses these to seed the QSS candidates' `POP_KDC` /
`POP_KINT`; without them the stiff TMDD ODE diverges. Used internally by
[`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
when `tool = "structsearch"`, `options$type = "tmdd"` and `options$kd`
is supplied. If the results already carry `POP_KM`/`POP_CLMM` (an
MM-parameterised base) the object is returned unchanged.

## Usage

``` r
seed_tmdd_results(results, kd, verbose = TRUE)
```

## Arguments

- results:

  a Pharmpy `ModelfitResults` object (e.g. from
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)).

- kd:

  target dissociation constant seed (concentration units).

- verbose:

  emit an info message?

## Value

a `ModelfitResults` with augmented `parameter_estimates`.
