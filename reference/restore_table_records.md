# Restore \$TABLE records in a Pharmpy NONMEM model object

Replaces all \$TABLE blocks in the model code with the saved table
records, then re-parses the model. This is used to work around pharmpy
bugs that corrupt \$TABLE records during estimation step updates.

## Usage

``` r
restore_table_records(model, saved_tables)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- saved_tables:

  character vector of \$TABLE lines as returned by
  [`get_table_records()`](https://insightrx.github.io/pharmr.extra/reference/get_table_records.md)

## Value

updated pharmpy model object
