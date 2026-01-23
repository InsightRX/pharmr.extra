# Export a pharmpy model object.

Pharmpy model objects loaded in R cannot be stored and retrieved
currently using native Pharmpy code. This is a (hopefully temporary)
wrapper function to export the model and dataset to an RDS file.

## Usage

``` r
export_pharmpy_model(model, file)
```

## Arguments

- model:

  Pharmpy model object to save

- file:

  RDS file to save to

## Value

TODO
