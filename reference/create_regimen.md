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
} # }
```
