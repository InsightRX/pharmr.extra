# Create a Pharmpy model object from a NONMEM report file (`.lst`/`.res`)

For the case where only a NONMEM report file is available and the `.ext`
file (from which pharmpy normally reads final estimates) is missing.
Reads the embedded control stream and applies the final parameter
estimates parsed from the report's `FINAL PARAMETER ESTIMATE` section.

## Usage

``` r
create_model_from_output(output_file, data = NULL, save_as = NULL, verbose = TRUE)
```

## Arguments

- output_file:

  path to a NONMEM report file (`.lst`, `.res`, ...).

- data:

  optional dataset (filename or `data.frame`) passed to
  [`create_model_from_file()`](https://insightrx.github.io/pharmr.extra/reference/create_model_from_file.md).

- save_as:

  optional path to write the resulting NONMEM model code to. Default
  `NULL` (return the model object only).

- verbose:

  verbose output. Default `TRUE`.

## Value

a Pharmpy model object.

## Details

Internally the parsed estimates are written to a synthetic `.ext` file
and fed through the existing
[`create_model_from_file()`](https://insightrx.github.io/pharmr.extra/reference/create_model_from_file.md)
path, so pharmpy handles the NONMEM-name to parameter-name mapping.

**Precision:** estimates recovered from a report file are rounded to ~3
significant figures (see
[`parse_output()`](https://insightrx.github.io/pharmr.extra/reference/parse_output.md)).
Use an `.ext` file with
[`create_model_from_file()`](https://insightrx.github.io/pharmr.extra/reference/create_model_from_file.md)
when full precision is required.

## See also

[`parse_output()`](https://insightrx.github.io/pharmr.extra/reference/parse_output.md),
[`create_model_from_file()`](https://insightrx.github.io/pharmr.extra/reference/create_model_from_file.md)
