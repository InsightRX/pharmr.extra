# Wrapper around pharmr's functions to set/add estimation methods

The current pharmpy functionality is not stable, hence the need for this
wrapper.

## Usage

``` r
update_estimation_method(
  model,
  estimation_method,
  uncertainty_method = "none",
  per_step_options = NULL,
  tool = "nonmem",
  verbose = TRUE
)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- estimation_method:

  Optional. Character vector of estimation method(s) to apply to model.
  Will remove all existing estimation steps in the model and update with
  methods specified in argument.

- uncertainty_method:

  parameter uncertainty method passed to Pharmpy for newly added
  estimation steps (e.g. `"SANDWICH"`, `"SMAT"`). Default `"none"` adds
  no uncertainty step.

- per_step_options:

  optional list with one element per estimation method, each a named
  list of estimation options for that step (as produced by
  [`parse_estimation_options()`](https://insightrx.github.io/pharmr.extra/reference/parse_estimation_options.md)).
  `NULL` (default) uses the default options for each method.

- tool:

  estimation tool, `"nonmem"` (default) or `"nlmixr"`. Determines the
  default estimation options and whether NONMEM `$COV`/`$TABLE` records
  are preserved.

- verbose:

  verbose output?

## Value

Pharmpy model object with updated estimation step(s).
