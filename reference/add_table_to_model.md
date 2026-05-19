# Add new \$TABLE record to output variables

Add new \$TABLE record to output variables

## Usage

``` r
add_table_to_model(
  model,
  variables,
  firstonly = FALSE,
  file,
  reload_dataset = TRUE
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

## Value

TODO
