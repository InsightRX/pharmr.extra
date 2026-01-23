# Export a pharmpy model object

Currently, pharmr/pharmpy is not able to export results objects from
grid searches or model fits. This function aims to work around that
until it does. It essentially saves the model information separately,
and upon reloading it reconstructs the models in the results object.

## Usage

``` r
export_pharmpy_results(results, file)
```

## Arguments

- results:

  Pharmpy results object to save

- file:

  JSON file to save to

## Value

TODO

## Details

The saved object is not a Pharmpy object, but it has most of the same
entries so can be used as a drop-in.
