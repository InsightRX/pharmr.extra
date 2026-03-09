# Get route from data. If dose and observation events all happen in the same compartment, then assume IV administration, else oral absorption (or sc, im, etc).

Get route from data. If dose and observation events all happen in the
same compartment, then assume IV administration, else oral absorption
(or sc, im, etc).

## Usage

``` r
get_route_from_data(data, default = "iv")
```

## Arguments

- data:

  filename of dataset or data.frame as input to NONMEM / nlmixr.

- default:

  default route (`iv` by default)

## Value

route (character)
