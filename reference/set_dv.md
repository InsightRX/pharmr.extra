# Set the dependent variable (DV) column in a Pharmpy model's datainfo

Updates the `datainfo` object so that the specified column has type
`'dv'`. Any column that previously had type `'dv'` is demoted to type
`'unknown'`.

## Usage

``` r
set_dv(model, dv)
```

## Arguments

- model:

  Pharmpy model object

- dv:

  Name of the column to set as the dependent variable

## Value

Pharmpy model object with updated datainfo
