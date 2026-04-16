# Validate the specified model, ensure it's valid Pharmpy model

Validate the specified model, ensure it's valid Pharmpy model

## Usage

``` r
validate_model(model, data = NULL)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- data:

  filename of dataset or data.frame as input to NONMEM / nlmixr.
  Optional, can also be included in `model` object (if specified as
  pharmpy model object).
