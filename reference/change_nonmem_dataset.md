# Change \$DATA in NONMEM model code

Thin wrapper around
[`update_nonmem_data()`](https://insightrx.github.io/pharmr.extra/reference/update_nonmem_data.md)
kept for internal callers. Always returns a single string regardless of
the input shape.

## Usage

``` r
change_nonmem_dataset(code, path)
```

## Arguments

- code:

  model code, either as single line string, or vector of lines

- path:

  path of new dataset
