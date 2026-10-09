# Resolve an on-disk dataset path from a model's \$DATA record

Parses the \$DATA record of a NONMEM model and returns the first element
that is an existing file on disk (ignoring `IGNORE=`/`ACCEPT=` options).
Relative paths are resolved against `base_dir`, the folder the model
file was read from, rather than the working directory or the run folder.
Returns `NULL` when no element points to an existing file (e.g. \$DATA
is the `DUMMYPATH` placeholder used while the dataset lives only in
memory).

## Usage

``` r
get_dataset_path_from_model(model, base_dir = getwd())
```

## Arguments

- model:

  pharmpy model object

- base_dir:

  directory relative `$DATA` paths are resolved against. Defaults to the
  working directory.

## Value

path to an existing dataset file (character), or `NULL`. The returned
path carries an attribute `absolute`, `TRUE` when `$DATA` already held
an absolute path.
