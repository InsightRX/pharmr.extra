# Set the dependent variable (DV) column in a Pharmpy model's datainfo

Updates the `$INPUT` record in the NONMEM model code so that the
specified column is mapped to NONMEM's internal `DV` variable, and
updates the `datainfo` accordingly. Any column that previously had type
`'dv'` is demoted to type `'unknown'`.

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

Pharmpy model object with updated datainfo and \$INPUT record
