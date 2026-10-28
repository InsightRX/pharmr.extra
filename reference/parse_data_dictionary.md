# Create a dictionary from given specs and default dictionary as fallback

Create a dictionary from given specs and default dictionary as fallback

## Usage

``` r
parse_data_dictionary(
  dictionary,
  default = list(ID = "ID", TIME = "TIME", DV = "DV", EVID = "EVID", AMT = "AMT", CMT =
    "CMT", MDV = "MDV")
)
```

## Arguments

- dictionary:

  named list mapping standard column names (e.g. `ID`, `TIME`) to the
  column names used in the dataset.

- default:

  named list with the default mapping, used for any entry not given in
  `dictionary`.

## Value

named list: `default` with the entries in `dictionary` overriding.
