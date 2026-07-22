# Parse a NONMEM report file (`.lst`/`.res`)

Extracts the embedded control stream and the final parameter estimates
(THETA vector, OMEGA and SIGMA covariance matrices, and the objective
function value) from a NONMEM report/output file. This is useful when
only the report file is available and the `.ext` file (which pharmpy
normally reads final estimates from) is missing.

## Usage

``` r
parse_output(output_file = NULL, code = NULL)
```

## Arguments

- output_file:

  path to a NONMEM report file (`.lst`, `.res`, ...).

- code:

  character string with the report file contents (alternative to
  `output_file`).

## Value

a list with elements `control_stream` (character), `theta` (numeric
vector), `omega` (numeric matrix or `NULL`), `sigma` (numeric matrix or
`NULL`), and `ofv` (numeric scalar or `NA`).

## Details

**Precision:** the `FINAL PARAMETER ESTIMATE` block in a `.lst` is
rounded to roughly three significant figures, so estimates recovered
this way are lower precision than those in the corresponding `.ext`
file. Use the `.ext` file (via
[`create_model_from_file()`](https://insightrx.github.io/pharmr.extra/reference/create_model_from_file.md))
when full precision is required.

Standard errors and covariance-step output are not parsed. OMEGA/SIGMA
parsing handles the single column block NONMEM prints for up to six
random effects; wider matrices, which NONMEM splits across multiple
column blocks, are not yet supported.
