# Compare NLME output from last n runs

The function will scan through the folder and look for modelfit folders
(using `filter`). It will then order these folders by date and select
the last `n`. It will then generate comparison tables for the runs, one
for general run info, and one for the parameter estimates.

## Usage

``` r
compare_nlme_runs(
  filter = "run",
  folder = ".",
  n = 3,
  save_info = NULL,
  save_parameters = NULL
)
```

## Arguments

- filter:

  the filter to apply to scan for results

- folder:

  folder to scan for NLME results

- n:

  the last n results to compare

- save_info:

  save general run info as a csv file

- save_parameters:

  save run parameters as a csv file

## Value

TODO
