# Function to remove specific NONMEM records from model file

Function to remove specific NONMEM records from model file

## Usage

``` r
remove_nonmem_records(text, short_name = "EST")
```

## Arguments

- text:

  NONMEM model code

- short_name:

  shortest name of NONMEM record to regex match. E.g. `EST` for
  \$ESTIMATION records, since this may be abbreviated to \$EST.

## Value

NONMEM model code
