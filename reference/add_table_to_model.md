# Add new \$TABLE record to output variables

Add new \$TABLE record to output variables

## Usage

``` r
add_table_to_model(
  model,
  variables,
  firstonly = FALSE,
  file,
  reload_dataset = TRUE,
  id_format = "sF11.0",
  format = NULL
)
```

## Arguments

- model:

  pharmpy model object

- variables:

  character vector with variable names

- firstonly:

  add `FIRSTONLY` parameter to \$TABLE record

- file:

  path to file, e.g. `sdtab`

- reload_dataset:

  should dataset be reloaded into the Pharmpy model object after
  updating the model. Default is TRUE, to ensure a proper Pharmpy model
  object, but can result in issues.

- id_format:

  NONMEM `$TABLE` output format for the `ID` column only. Defaults to
  `sF11.0`, which writes integer subject IDs of up to 10 digits in full
  (NONMEM's default format truncates IDs to ~6-7 significant digits).
  Set to `NULL` to leave the ID column at NONMEM's default format.

- format:

  NONMEM `$TABLE` output format for *all* columns. `NULL` (default)
  leaves NONMEM's default (`s1PE11.4`), which is what you almost always
  want: a fixed-point format such as `sF9.0` rounds every column in the
  table (concentrations, times, parameters) to that many decimals. Note
  that NONMEM carries `FORMAT` over to all subsequent `$TABLE` records,
  so setting it here also affects tables added later.

## Value

TODO
