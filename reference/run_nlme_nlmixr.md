# Run a pharmpy nlmixr-format model with nlmixr2

Internal companion to
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md);
called when the input model is in pharmpy's nlmixr backend. The function
turns the pharmpy nlmixr model into an `rxode2`/`nlmixr2` function, fits
it with `nlmixr2::nlmixr2()`, and wraps the result in a list that mimics
the shape of a pharmpy `ModelfitResults` so downstream helpers
([`attach_fit_info()`](https://insightrx.github.io/pharmr.extra/reference/attach_fit_info.md),
[`compare_nlme_fit()`](https://insightrx.github.io/pharmr.extra/reference/compare_nlme_fit.md),
etc.) work without engine-specific branches.

## Usage

``` r
run_nlme_nlmixr(
  model,
  data = NULL,
  id,
  path = getwd(),
  estimation_method = NULL,
  control = NULL,
  force = NULL,
  save_fit = TRUE,
  save_summary = TRUE,
  save_final = TRUE,
  clean = TRUE,
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

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- path:

  path to nonmem model. If not specified, will assume current working
  directory.

- estimation_method:

  Optional. Character vector of estimation method(s) to apply to model.
  Will remove all existing estimation steps in the model and update with
  methods specified in argument.

- control:

  nlmixr2-only. Optional control list passed verbatim to
  `nlmixr2::nlmixr2()` (e.g.
  [`nlmixr2est::foceiControl()`](https://nlmixr2.github.io/nlmixr2est/reference/foceiControl.html)
  or
  [`nlmixr2est::saemControl()`](https://nlmixr2.github.io/nlmixr2est/reference/saemControl.html)).
  Ignored for NONMEM models.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- save_fit:

  save fit object. If `TRUE`, will save as `<id>.rds` inside `path`. Can
  also specify a filename (rds) to save to; relative filenames are
  resolved against `path`, absolute ones are used as-is. `FALSE` writes
  nothing.

- save_summary:

  save fit summary and parameter estimates to file? Default is `TRUE`.
  Files are written to `path` as `<id>_fit_summary.txt` and
  `<id>_fit_parameters.csv`.

- save_final:

  after running the model, should a file `final.mod` be created with the
  final estimates from the run.

- clean:

  clean up run folder after NONMEM execution?

- mu_reference:

  Controls mu-referencing for SAEM models. `"auto"` (default)
  automatically applies
  [`pharmr::mu_reference_model()`](https://rdrr.io/pkg/pharmr/man/mu_reference_model.html)
  when SAEM is used and the model is not already mu-referenced. `TRUE`
  always applies mu-referencing. `FALSE` never applies mu-referencing
  (old behaviour: warns when SAEM is used without mu-referencing).

- verbose:

  verbose output?

## Details

Pharmpy can in principle drive nlmixr2 itself, but that path requires
the Python `pyreadr` module which is not part of the standard install;
calling nlmixr2 directly is also faster (no R→Python→R round-trip).
