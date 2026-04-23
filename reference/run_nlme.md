# Run model in NONMEM

Run the model directly using nmfe (not through pharmpy). This is a more
reliable way of running NONMEM, and it is now possible to stream stdout
and stderr to file or to console, which is useful for inspection of
intermediate model fit.

## Usage

``` r
run_nlme(
  model,
  data = NULL,
  tables = NULL,
  full_tables = FALSE,
  id,
  path = getwd(),
  method = c("nmfe", "pharmpy", "psn"),
  nmfe = get_nmfe_location(),
  force = NULL,
  console = FALSE,
  save_fit = TRUE,
  save_summary = TRUE,
  estimation_method = NULL,
  estimation_options = NULL,
  sir_options = NULL,
  auto_stack_encounters = TRUE,
  clean = TRUE,
  as_job = FALSE,
  save_final = TRUE,
  check_only = FALSE,
  remove_tables = FALSE,
  mu_reference = "auto",
  verbose = TRUE
)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- data:

  filename of dataset or data.frame as input to NONMEM / nlmixr.
  Optional, can also be included in `model` object (if specified as
  pharmpy model object).

- tables:

  acharacter vector of which default tables to add, options are `fit`
  and `parameters`. Default is NULL, i.e. don't add any new tables (but
  will keep existing).

- full_tables:

  For the default tables, should all input columns from be included in
  the output tables? Default `FALSE`.

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- path:

  path to nonmem model. If not specified, will assume current working
  directory.

- method:

  run method, either `pharmpy` dispatch, `nmfe` or `psn` (psn::execute).

- nmfe:

  full path to nmfe file to run NONMEM with, if `method=="nmfe"`.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- console:

  show stderr and stdout in R console? If FALSE, will stream to files
  `stdout` and `stderr` in fit folder.

- save_fit:

  save fit object. If `TRUE`, will save as \<run_id.rds\>. Can also
  specify filename (rds) to save to.

- save_summary:

  save fit summary and parameter estimates to file? Default is `TRUE`.
  Will use current folder, and save as `fit_summary_<id>.txt` and
  `fit_parameters_<id>.csv`.

- estimation_method:

  Optional. Character vector of estimation method(s) to apply to model.
  Will remove all existing estimation steps in the model and update with
  methods specified in argument.

- estimation_options:

  Optional. Options for the estimation step(s). Either a flat named list
  (applied to the first step) or a named list of lists keyed by method
  name for multi-step estimation, e.g.
  `list(SAEM = list(NBURN = 500), IMP = list(NITER = 10))`. Options are
  merged with package defaults; user values take precedence. Keys that
  correspond to pharmpy structured fields (MAXEVAL, NITER, ISAMPLE,
  PRINT, AUTO, ETASAMPLES) are routed to the appropriate attribute to
  avoid duplication in the rendered `$EST` record.

- sir_options:

  options for running SIR in covariance step. A list with options
  `niter` (number of SIR iterations) and `samples` (number of samples).
  Default `NULL` leaves the model unchanged. `samples` should be between
  300 and 10000 (suggested to use 1000 by default). `niter` should be 1
  or higher (suggest to use 1 by default).

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

- clean:

  clean up run folder after NONMEM execution?

- as_job:

  run as RStudio job?

- save_final:

  after running the model, should a file `final.mod` be created with the
  final estimates from the run.

- check_only:

  if `TRUE`, will only check the model code (NM-TRAN in the case of
  NONMEM), but not run the model. Will return `TRUE` if model syntax is
  correct, and `FALSE` if not. Will also attach stdout as `message`
  attribute.

- remove_tables:

  if `TRUE`, removes all `$TABLE` records from the model before running.
  Applied after any tables added via the `tables` argument. Default is
  `FALSE`.

- mu_reference:

  Controls mu-referencing for SAEM models. `"auto"` (default)
  automatically applies
  [`pharmr::mu_reference_model()`](https://rdrr.io/pkg/pharmr/man/mu_reference_model.html)
  when SAEM is used and the model is not already mu-referenced. `TRUE`
  always applies mu-referencing. `FALSE` never applies mu-referencing
  (old behaviour: warns when SAEM is used without mu-referencing).

- verbose:

  verbose output?

## Value

TODO

## Details

The function does take a pharmpy model as input (optionally), and uses
pharmpy to read the results from the model fit, and returns a pharmpy
`modelfit` object.
