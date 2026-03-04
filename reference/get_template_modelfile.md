# Helper function to get the name of the template modelfile to load, based on route and ODE / analytical

Helper function to get the name of the template modelfile to load, based
on route and ODE / analytical

## Usage

``` r
get_template_modelfile(route, n_cmt, force_ode)
```

## Arguments

- route:

  route of administration, either `oral` or `iv`

- n_cmt:

  number of elimination and distribution compartments. Default is 1,
  i.e. no peripheral distributions.

- force_ode:

  force creation of a model with ODEs, even though the model is linear.
  Can be `FALSE` (default), `TRUE`, or ADVAN number (for NONMEM models).
  In the latter case, options are either `6`, `9`, or `13`.

## Value

modelfile name (character)
