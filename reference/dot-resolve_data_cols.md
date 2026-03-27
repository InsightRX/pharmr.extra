# Resolve data column names from an explicit data argument or \$DATA filename

Returns a character vector of column names (length `n_input`), or `NULL`
if no usable data source was found.

## Usage

``` r
.resolve_data_cols(data, data_lines, model_dir, n_input)
```

## Arguments

- data:

  User-supplied `data` argument (data.frame, file path, or NULL).

- data_lines:

  Lines of the `$DATA` record from the parsed model.

- model_dir:

  Directory of the model file, used to resolve relative paths.

- n_input:

  Number of `$INPUT` entries expected.
