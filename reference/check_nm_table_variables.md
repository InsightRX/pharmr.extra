# Check NONMEM table variables

Check NONMEM table variables

## Usage

``` r
check_nm_table_variables(model, variables, throw_error = TRUE)
```

## Arguments

- model:

  pharmpy model object

- variables:

  character vector with variable names

- throw_error:

  should throw error? If FALSE, will return vector of invalid variables.

## Value

`NULL` if all variables are valid, otherwise an error (or return all
invalid variables if `throw_error` is FALSE).
