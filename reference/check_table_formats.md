# Validate the FORMAT / IDFORMAT options of a \$TABLE record

Validate the FORMAT / IDFORMAT options of a \$TABLE record

## Usage

``` r
check_table_formats(id_format, format = NULL)
```

## Arguments

- id_format:

  `IDFORMAT` spec, or `NULL`

- format:

  `FORMAT` spec, or `NULL` for the NONMEM default (`s1PE11.4`)

## Value

`NULL`, invisibly; called for the error it may throw.
