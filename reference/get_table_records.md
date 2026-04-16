# Extract \$TABLE records from a Pharmpy NONMEM model object

Extract \$TABLE records from a Pharmpy NONMEM model object

## Usage

``` r
get_table_records(model)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

## Value

character vector of \$TABLE blocks, or NULL if no tables exist
