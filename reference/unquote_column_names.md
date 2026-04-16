# Strip surrounding quote characters from data.frame column names

Pharmpy rejects column names that are not valid Python identifiers, and
NONMEM misinterprets CSV headers that start with a quote character. This
helper removes a single pair of matching surrounding single or double
quotes from each column name.

## Usage

``` r
unquote_column_names(data)
```

## Arguments

- data:

  data.frame (or NULL / non-data.frame, returned unchanged)

## Value

data.frame with sanitised column names, or the input unchanged
