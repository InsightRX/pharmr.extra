# Find input variables referenced in dose-timing parameter assignments

Scans comment-stripped model code for lines where a dose-timing
parameter (`D<n>`, `ALAG<n>`, `F<n>`, `R<n>`) is on the left-hand side
of an assignment, then extracts every identifier on the right-hand side
that is also an `$INPUT` column. This covers both simple assignments
(`D1 = DUR`) and expressions (`D1 = DUR * 24`).

## Usage

``` r
.find_dose_variables(model_code, input_names)
```

## Arguments

- model_code:

  Comment-stripped model code string.

- input_names:

  Character vector of NONMEM names from `$INPUT`.
