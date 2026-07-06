# Strip commas from the \$INPUT record of NONMEM model code

Comma-separated `$INPUT` items are valid NONMEM but not accepted by
Pharmpy, which expects space-separated column names. This replaces
commas inside the `$INPUT` record with spaces, leaving other records
(e.g. the comma-containing `$THETA`/`$OMEGA` bounds) untouched. Matching
stops at the next record boundary (`$`).

## Usage

``` r
strip_input_commas(text)
```

## Arguments

- text:

  Character string with model code.

## Value

Character string with model code.
