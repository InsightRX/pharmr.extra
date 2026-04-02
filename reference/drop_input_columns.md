# Drop columns from the \$INPUT record in a NONMEM model

Replaces standalone column names in the \$INPUT record with DROP.

## Usage

``` r
drop_input_columns(model, columns)
```

## Arguments

- model:

  Pharmpy model object

- columns:

  character vector of column names to drop

## Value

Pharmpy model object with updated \$INPUT
