# Create basic goodness of fit plots based on nlmixr2 fit

Create basic goodness of fit plots based on nlmixr2 fit

## Usage

``` r
basic_gof_plot(fit, path = NULL)
```

## Arguments

- fit:

  fit object from nlmixr2

- path:

  if not NULL, path to save plot as file (file extension determines the
  file format automatically).

## Value

plotly object. If `path` is specified, will not return anything, but
plotly file will be saved at the requested path.
