# Update the dataset path in NONMEM model code

Replaces the dataset path in the first `$DATA` record with a new path,
while preserving every other element: any options on the `$DATA` line
(`IGNORE=@`, `ACCEPT=(...)`, `REWIND`, `RECORDS=N`, ...), any inline
comment on that line, and all other records in the model code.

## Usage

``` r
update_nonmem_data(code, path)
```

## Arguments

- code:

  NONMEM model code. Either a single character string (possibly with
  embedded newlines) or a character vector of lines.

- path:

  path to the new dataset.

## Value

The updated model code, in the same shape as `code`: a single string if
`code` was a string, a character vector otherwise.

## Details

The match is case-insensitive and also accepts the abbreviated `$DAT`
record name. Dataset paths in the original record may be unquoted or
wrapped in single or double quotes. If the new `path` contains
whitespace, it is wrapped in single quotes so NONMEM parses it as one
token.

## Examples

``` r
code <- "$PROB Test\n$DATA old.csv IGNORE=@ ACCEPT=(DV.GT.0)\n$INPUT ID TIME DV"
cat(update_nonmem_data(code, "new.csv"))
#> $PROB Test
#> $DATA new.csv IGNORE=@ ACCEPT=(DV.GT.0)
#> $INPUT ID TIME DV
```
