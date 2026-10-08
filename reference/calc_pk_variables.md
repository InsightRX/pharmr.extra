# Calculate some basic PK variables from simulated or observed data

AUC_SS is derived per subject as `F * dose / CL`, from the subject's
last dose in `regimen`, the bioavailability of the compartment that dose
goes into, and the subject's `CL`, then scaled by `dv_scale`.

## Usage

``` r
calc_pk_variables(data, regimen = NULL, dv_scale = 1, bioavailability = NULL)
```

## Arguments

- data:

  data.frame in NONMEM format

- regimen:

  dosing regimen as a list with a `dose` element, used to derive AUC_SS
  from the last dose. When it also has an `id` element (the subject each
  dose belongs to) AUC_SS uses each subject's own last dose, otherwise
  the last dose overall is used for every subject. An optional `cmt`
  element gives the compartment each dose goes into, which decides the
  bioavailability that applies (where absent, the regimen's
  `default_cmt`, else compartment 1), and an optional `time` element the
  dose time, at which that bioavailability is taken. `NULL` skips
  AUC_SS.

- dv_scale:

  multiplier putting `dose / CL` into the units the model reports
  concentrations in, i.e. `V / S<n>` for the observation compartment
  (see
  [`get_dv_scale_factor()`](https://insightrx.github.io/pharmr.extra/reference/get_dv_scale_factor.md)).
  `1` (the default) is the usual `S<n> = V` case.

- bioavailability:

  named character vector mapping a dose compartment (number or name, as
  in the `CMT` of the dose records) to the column in `data` holding the
  bioavailability for that compartment. `NULL` (the default) maps
  compartment `n` to a column `F<n>` (as NONMEM names them, in either
  case) where `data` has one. Doses into a compartment without a
  bioavailability column are taken as fully bioavailable.

## Value

data.frame
