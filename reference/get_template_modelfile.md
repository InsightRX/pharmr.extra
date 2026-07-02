# Helper function to get the name of the template modelfile to load, based on route and ODE / analytical

Helper function to get the name of the template modelfile to load, based
on route and ODE / analytical

## Usage

``` r
get_template_modelfile(route, n_cmt, force_ode)
```

## Arguments

- route:

  route of administration. One of `"auto"` (default), `"oral"`, `"sc"`,
  `"im"`, `"extravascular"`, or `"iv"`. When `"auto"`, the route is
  inferred from `data` by comparing the `CMT` of dose events (`EVID=1`)
  to the `CMT` of observation events (`EVID=0`): if any dose compartment
  is not also an observation compartment (e.g. doses in `CMT=1`,
  observations in `CMT=2`), the route is set to `"oral"`; otherwise
  `"iv"`. Falls back to `"iv"` if `data` is `NULL` or the dataset lacks
  the required `CMT`/`EVID` columns.

- n_cmt:

  number of elimination and distribution compartments. Default is 1,
  i.e. no peripheral distributions.

- force_ode:

  force creation of a model with ODEs, even though the model is linear.
  Can be `FALSE` (default), `TRUE`, or ADVAN number (for NONMEM models).
  In the latter case, options are either `6`, `9`, or `13`.

## Value

modelfile name (character)
