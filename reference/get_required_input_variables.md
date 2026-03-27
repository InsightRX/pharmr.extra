# Get required input variables for a NONMEM model

Parses a NONMEM model and determines which variables from `$INPUT` are
required to create a new input dataset. Variables are classified as:

- `"reserved"` – standard NONMEM data items with intrinsic meaning (ID,
  TIME, DV, AMT, EVID, etc.)

- `"dose_variable"` – columns referenced on the right-hand side of a
  dose-timing parameter assignment (`D1`–`D9`, `ALAG1`–`ALAG9`,
  `F1`–`F9`, `R1`–`R9`) in `$PK` (e.g. `D1 = DUR` or `D1 = DUR * 24`).
  These must be specified per dose event, not per subject.

- `"used_covariate"` – non-reserved columns explicitly referenced in the
  model code (`$PK`, `$DES`, `$ERROR`, `$PRED`) but not classified as a
  dose variable

- `"unused_covariate"` – columns present in `$INPUT` but never
  referenced in model code

- `"dropped"` – columns marked `DROP` in `$INPUT`

## Usage

``` r
get_required_input_variables(
  model,
  data = NULL,
  include_reserved_nonmem = TRUE
)
```

## Arguments

- model:

  Path to a NONMEM `.mod`/`.ctl` file, NONMEM model code as a single
  string, or a Pharmpy NONMEM model object.

- data:

  Optional. A data.frame or path to a CSV file whose column names are
  used to populate `data_col`. Columns are matched to `$INPUT` entries
  positionally (same order). When omitted the filename from `$DATA` is
  tried automatically; if that file cannot be found, `data_col` falls
  back to `nonmem_name`.

- include_reserved_nonmem:

  Logical. If `TRUE` (default), reserved NONMEM variables are included
  in the returned data frame. Set to `FALSE` to return only
  covariate-type variables (useful when you only need to know which
  subject-level covariates to include).

## Value

A `data.frame` with columns:

- nonmem_name:

  Name used inside NONMEM model code.

- data_col:

  Corresponding column name in the data file.

- type:

  Classification: `"reserved"`, `"dose_variable"`, `"used_covariate"`,
  `"unused_covariate"`, or `"dropped"`.

- required:

  `TRUE` if the column must be present in a new input dataset.

## Details

`"reserved"`, `"dose_variable"`, and `"used_covariate"` columns are all
considered required for simulation. Renames in `$INPUT` (e.g.
`WT=WEIGHT`, where `WT` is the NONMEM internal name and `WEIGHT` is the
data-file column) are handled correctly.
