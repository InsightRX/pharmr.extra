# Create a single regimen

The resulting data.frame can be passed to
[`create_sim_dataset()`](https://insightrx.github.io/pharmr.extra/reference/create_sim_dataset.md)
as the `regimen` argument.

## Usage

``` r
create_regimen(
  dose,
  interval = 24,
  n,
  t_inf = NULL,
  per = NULL,
  route = c("oral", "iv", "sc", "im")
)
```

## Arguments

- dose:

  TODO

- interval:

  TODO

- n:

  TODO

- t_inf:

  TODO

- per:

  character or `NULL`. Name of a column in the simulation dataset whose
  value is used to scale each subject's dose (e.g. `per = "WT"` for
  mg/kg dosing, `per = "BSA"` for mg/m² dosing). The final AMT for each
  subject is `dose * covariate_value`. When `NULL` (the default), the
  dose is applied as an absolute amount.

- route:

  TODO

## Value

TODO

## Examples

``` r
if (FALSE) { # \dontrun{
reg1 <- create_regimen(
  dose = 500,
  interval = 12,
  n = 10,
  route = "oral"
)
create_sim_dataset(..., regimen = reg1)

# Weight-based dosing (5 mg/kg):
reg2 <- create_regimen(dose = 5, per = "WT", interval = 24, n = 5, route = "sc")
create_sim_dataset(..., regimen = reg2)
} # }
```
