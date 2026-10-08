# Calculate some basic PK variables from simulated or observed data

Calculate some basic PK variables from simulated or observed data

## Usage

``` r
calc_pk_variables(data, regimen = NULL, dv_scale = 1)
```

## Arguments

- data:

  data.frame in NONMEM format

- regimen:

  dosing regimen as a list with a `dose` element, used to derive AUC_SS
  from the last dose. `NULL` skips AUC_SS.

- dv_scale:

  multiplier putting `dose / CL` into the units the model reports
  concentrations in, i.e. `V / S<n>` for the observation compartment
  (see
  [`get_dv_scale_factor()`](https://insightrx.github.io/pharmr.extra/reference/get_dv_scale_factor.md)).
  `1` (the default) is the usual `S<n> = V` case.

## Value

data.frame
