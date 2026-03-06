# Create a Pharmpy model object from a model file and dataset (optional)

Create a Pharmpy model object from a model file and dataset (optional)

## Usage

``` r
create_model_from_file(
  model_file,
  ext_file = NULL,
  data = NULL,
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

- verbose:

  verbose output

## Value

a Pharmpy model object
