# NONMEM output table import function

Quickly import NONMEM output tables into R. Function taken from
`modelviz` package by Benjamin Guiastrennec. When both `skip` and
`header` are `NULL`, `read_nmtab` will automatically detect the optimal
settings to import the tables. When more than one files are provided for
a same NONMEM run, they will be combined into a single `data.frame`.

## Usage

``` r
read_table_nm(
  file = NULL,
  skip = NULL,
  header = NULL,
  rm_duplicates = FALSE,
  nonmem_tab = TRUE,
  subproblems = FALSE
)
```

## Arguments

- file:

  full file name

- skip:

  number of lines to skip before reading data

- header:

  logical value indicating whether the file contains the names of the
  variables as its first line

- rm_duplicates:

  logical value indicating whether duplicated columns should be removed

- nonmem_tab:

  logical value indicating to the function whether the file is a table
  or a nonmem additional output file.

- subproblems:

  keep the simulation subproblems apart. A table written by a
  `$SIMULATION` record with `SUBPROBLEMS > 1` holds one block of rows
  per subproblem, each introduced by a repeated `TABLE NO.` header. By
  default those headers are discarded and the blocks are returned as one
  undivided concatenation. With `subproblems = TRUE` the table is split
  on them instead and a 1-based `.subproblem` column is added. Only
  supported for a single `file` and `nonmem_tab = TRUE`.

## Value

A `data.frame`

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_table_nm(file = '../models/pk/sdtab101')
} # }
```
