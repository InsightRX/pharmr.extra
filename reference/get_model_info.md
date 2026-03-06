# Get model info

The `get_*` family of functions are designed to get different types of
model information from a Pharmpy model.

## Usage

``` r
get_model_info(model)

get_error_model(model)

get_elimination(model)

get_absorption(model)

get_ode_linearity(model)

get_estimation_steps(model)
```

## Arguments

- model:

  Either a Pharmpy model object, a path to a model file, a string
  containing NONMEM model code, or a pharmpy model stored using
  [`export_pharmpy_model()`](https://insightrx.github.io/pharmr.extra/reference/export_pharmpy_model.md).
  Model files, strings, and stored models will be coerced to a Pharmpy
  model.

## Value

For `get_model_info()`: a named list providing a structured summary of
the model. For other `get_*` functions: a character string.
