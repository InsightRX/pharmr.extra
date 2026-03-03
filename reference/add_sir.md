# Add SIR sampling in covariance step in Pharmpy model

Add SIR sampling in covariance step in Pharmpy model

## Usage

``` r
add_sir(model, options = list(niter = 1, samples = 1000))
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- options:

  SIR options, one of `iter`, `samples`
