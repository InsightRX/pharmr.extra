# Combine columns with run info into a data.frame and make sure that rows match (e.g. parameters)

data.frames in list should have the same column names but can have
different row names (e.g. parameter names).

## Usage

``` r
combine_info_columns(fit_info, table = "info_tab", label = "Detail")
```

## Arguments

- fit_info:

  list with one element per fit, each a list with elements `name` (run
  name), `info_tab` and `par_tab` (as created in
  [`compare_nlme_fit()`](https://insightrx.github.io/pharmr.extra/reference/compare_nlme_fit.md)).

- table:

  name of the table element to combine, `"info_tab"` (default) or
  `"par_tab"`.

- label:

  label for the first column of the combined table.

## Value

data.frame with one column per fit, rows matched by name.
