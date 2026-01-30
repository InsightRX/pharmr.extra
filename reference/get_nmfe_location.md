# Helper function to determine nmfe location from various sources The order is as follows:

1.  argument specified by user

2.  check pharmpy config

3.  throw error, force user to specify

## Usage

``` r
get_nmfe_location(nmfe = NULL, verbose = FALSE)
```
