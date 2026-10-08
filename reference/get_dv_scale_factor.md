# Factor converting `dose / CL` into the concentration units a model reports

NONMEM predicts `A(n) / S<n>` for the observation compartment, so a
model that reports ng/mL off an mg / L system writes `S2 = V2/1000`. AUC
at steady state *in those units* is `dose * (V / S) / CL`, not
`dose / CL`, so without this factor AUC_SS is off by exactly the scaling
applied in the model (1000 in that example).

## Usage

``` r
get_dv_scale_factor(
  model = NULL,
  compartment = NULL,
  code = NULL,
  verbose = FALSE
)
```

## Arguments

- model:

  Pharmpy model object. `NULL` (or a model whose scaling cannot be read)
  gives `1`.

- compartment:

  observation compartment number. Inferred with
  [`get_obs_compartment()`](https://insightrx.github.io/pharmr.extra/reference/get_obs_compartment.md)
  when `NULL` (the default). NONMEM models only.

- code:

  nlmixr2 / rxode2 model code (character), read instead of `model`. The
  scaling an nlmixr-format model applies lives only in its rendered code
  — `create_model(scale_observations = )` rewrites the prediction there
  as `S<n> <- <vol>/<scale>` — so the code is the only place it can be
  read back from, and it is also all a worker process has.

- verbose:

  verbose output?

## Value

single numeric multiplier.

## Details

Returns `1` — i.e. plain `dose / CL` — whenever no scaling applies or
none can be read off the model:

- no `S<n>` is defined for the observation compartment, in which case
  NONMEM scales by 1 and the model predicts amounts rather than
  concentrations,

- `S<n>` is the central volume itself (`S2 = V2`), the usual case,

- `S<n>` is something other than the central volume times or divided by
  a constant, which is reported as a warning since AUC_SS is then not
  `dose / CL` in any simple way,

- the model is an nlmixr-format Pharmpy model given without `code`: its
  statements carry no `S<n>` (see `code`).
