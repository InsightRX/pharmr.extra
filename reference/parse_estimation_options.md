# Parse estimation_options into a per-step list.

Parse estimation_options into a per-step list.

## Usage

``` r
parse_estimation_options(estimation_method, estimation_options)
```

## Arguments

- estimation_method:

  character vector of estimation method(s)

- estimation_options:

  flat list or named list of lists

## Value

a list of length `length(estimation_method)`, where each element is the
options list for that step (possibly empty).
