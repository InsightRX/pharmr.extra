# Coerce files, model code, and more to pharmpy model objects

`as_pharmpy_model()` turns an existing object, such as a NONMEM model
file or code, into a Pharmpy model.

## Usage

``` r
as_pharmpy_model(model)
```

## Arguments

- model:

  Either a Pharmpy model object, a path to a model file, a string
  containing NONMEM model code, or a pharmpy model stored using
  [`export_pharmpy_model()`](https://insightrx.github.io/pharmr.extra/reference/export_pharmpy_model.md).
  Model files, strings, and stored models will be coerced to a Pharmpy
  model.

## Value

A Pharmpy model.
