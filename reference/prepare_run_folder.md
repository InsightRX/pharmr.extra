# Create a folder for running model, with the model and dataset

Create a folder for running model, with the model and dataset

## Usage

``` r
prepare_run_folder(
  id,
  model,
  path,
  force = FALSE,
  data = NULL,
  auto_stack_encounters = FALSE,
  copy_dataset = TRUE,
  verbose = TRUE
)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- path:

  path to folder in which to create the run folder `id`.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- data:

  filename of dataset or data.frame as input to NONMEM / nlmixr.
  Optional, can also be included in `model` object (if specified as
  pharmpy model object).

- auto_stack_encounters:

  only invoked if `data` argument supplied as a data.frame, not if a
  pharmpy model object is supplied without `data` or when `data` is a
  filename. Detects if TIME within an individual is decreasing from one
  record to another, which NONMEM cannot handle. If this happens, it
  will add a reset event (EVID=3) at that time, and increase the TIME
  for subsequent events so that NONMEM does not throw an error. It will
  increase the time for the next encounter to the maximum encounter
  length across all subjects in the dataset (rounded up to 100). If no
  decreasing TIME is detected, nothing will be done (most common case).
  This feature is useful e.g. for crossover trials when data on the same
  individual ispresent but is included in the dataset as time-after-dose
  and not actual time since first overall dose.

- copy_dataset:

  copy the dataset into the run folder? If `TRUE`, the dataset is copied
  into the run folder as `data.csv` and the model's `$DATA` record is
  rewritten to point to that copy. If `FALSE` (default), the dataset is
  left in its existing location and the model's `$DATA` record is
  rewritten to the dataset's absolute path (other `$DATA` options such
  as `IGNORE=` are kept), since NONMEM is run from inside the run folder
  where a relative path would not resolve. A `data` file path is
  resolved against the working directory; when `data` is not given, a
  relative `$DATA` path is resolved against the folder the model file
  was read from (or the working directory if unknown). A `$DATA` that
  already is an absolute path to an existing file is left as-is.
  `copy_dataset = FALSE` can only be honored when the dataset is a file
  on disk — i.e. `data` is supplied as a file path, or the model's
  `$DATA` record points to an existing file. If neither is the case
  (only an in-memory data frame, `model$dataset`, or original dataset is
  available), a warning is issued and the dataset is copied into the run
  folder (with `$DATA` rewritten) anyway.

- verbose:

  verbose output?
