# get nmfe file name from a NONMEM installation folder

get nmfe file name from a NONMEM installation folder

## Usage

``` r
detect_nmfe_version(nm_path)
```

## Arguments

- nm_path:

  path to the NONMEM installation folder (the folder that contains the
  `run/` subfolder).

## Value

file name of the first `nmfe<NN>` script found in `run/`, or `NA` if
none is found.
