# Remove all \$TABLE records from a model

Remove all \$TABLE records from a model

## Usage

``` r
remove_tables_from_model(model, file = NULL, reload_dataset = TRUE)
```

## Arguments

- model:

  pharmpy model object

- file:

  remove only a specific table defined as FILE=. `file` can also specify
  only the start of a filename, e.g. `patab`

- reload_dataset:

  should dataset be reloaded into the Pharmpy model object after
  updating the model. Default is TRUE, to ensure a proper Pharmpy model
  object, but can result in issues.

## Value

TODO
