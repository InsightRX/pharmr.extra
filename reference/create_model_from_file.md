# Create a Pharmpy model object from a model file and dataset (optional)

Create a Pharmpy model object from a model file and dataset (optional)

## Usage

``` r
create_model_from_file(
  model_file,
  ext_file = NULL,
  data = NULL,
  data_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- model_file:

  the model file (.mod) to read.

- ext_file:

  optional path to a .ext file containing final parameter estimates that
  will be used to update the initial estimates in the model.

- data:

  the filename of the dataset (or an actual data.frame)

- data_dir:

  directory a relative `$DATA` path in the model is relative to.
  Defaults to the folder `model_file` is in, as for NONMEM. Stored on
  the returned model as attribute `data_dir`, which
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  uses to point `$DATA` at the dataset's absolute path.

- verbose:

  verbose output

## Value

a Pharmpy model object
