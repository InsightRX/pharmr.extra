# Add a metabolite compartment, if requested

Add a metabolite compartment, if requested

## Usage

``` r
add_metabolite_compartment(mod, metabolite = FALSE, route)
```

## Arguments

- mod:

  Pharmpy model object.

- metabolite:

  either `TRUE`/`FALSE`, or a list with elements `drug_dvid` (integer)
  and `presystemic` (logical). See
  [`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md).

- route:

  route of administration (e.g. `"oral"`, `"iv"`). A presystemic
  metabolite cannot be combined with `route = "iv"`.

## Value

Pharmpy model object, with the metabolite compartment added if
requested.
