# Ensure the default `.ext` file exists in a NONMEM run folder

NONMEM's `$ESTIMATION FILE=...` option redirects the iteration output to
a non-default filename (e.g. PsN-style runs write `psn.ext`). pharmpy's
result reader derives the `.ext` path from the model stem (`run.mod` →
`run.ext`) and will fail silently when that file is missing or empty.

## Usage

``` r
ensure_default_ext_file(fit_folder, model_file, verbose = TRUE)
```

## Arguments

- fit_folder:

  path to the run folder.

- model_file:

  model filename (e.g. `"run.mod"`); the stem determines the expected
  `.ext` name.

- verbose:

  emit a `cli` message describing the action. Default `TRUE`.

## Value

Invisibly `TRUE` if a file was copied, `FALSE` otherwise.

## Details

This helper is called after the fit completes. If `<stem>.ext` is
missing or zero-bytes, and exactly one other non-empty `.ext` file is
present in the fit folder, that file is copied to `<stem>.ext`. When
zero or more than one candidate is found, nothing is copied.
