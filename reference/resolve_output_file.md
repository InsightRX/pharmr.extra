# Resolve an output filename against the run's `path`

Fit artefacts (`<id>.rds`, `<id>_fit_summary.txt`,
`<id>_fit_parameters.csv`) used to be written with bare relative names,
so they landed in [`getwd()`](https://rdrr.io/r/base/getwd.html) even
when the caller passed an explicit `path`. This helper anchors relative
names to `path` instead. Absolute paths are left untouched — POSIX
roots, `~`-prefixed paths, Windows drive letters and UNC shares — so an
explicit `save_fit = "/some/where/fit.rds"` still goes exactly where
asked.

## Usage

``` r
resolve_output_file(file, path = NULL)
```

## Arguments

- file:

  filename, possibly relative.

- path:

  folder to resolve relative filenames against. `NULL` or `NA` falls
  back to the working directory.

## Value

character path.
