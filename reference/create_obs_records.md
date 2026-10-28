# Create observation records, given a specified t_obs vector

Create observation records, given a specified t_obs vector

## Usage

``` r
create_obs_records(data, t_obs, n_subjects, model)
```

## Arguments

- data:

  simulation dataset (dose records), with at least `ID`, `EVID` and
  `.regimen` columns. Its first observation `CMT` (subject 1) is used as
  the observation compartment when present.

- t_obs:

  numeric vector of observation times.

- n_subjects:

  number of subjects to create observation records for.

- model:

  Pharmpy model object, used to determine the observation compartment
  when `data` does not provide one.

## Value

data.frame of observation records, one set of `t_obs` per subject and
regimen.
