# Add a `$PRIOR NWPRI` record set to a NONMEM model

Builds the NONMEM normal-inverse-Wishart prior records (`$PRIOR NWPRI`,
`$THETAP`, `$THETAPV`, `$OMEGAP`, `$OMEGAPD`, `$SIGMAP`, `$SIGMAPD`)
that let NONMEM itself draw parameter vectors from a fit's uncertainty
distribution, and inserts them into the model code.

## Usage

``` r
add_nwpri_prior(
  model,
  fit = NULL,
  plev = 0.9999,
  parameter_estimates = NULL,
  covariance_matrix = NULL
)
```

## Arguments

- model:

  a Pharmpy NONMEM model object.

- fit:

  a Pharmpy modelfit object carrying `parameter_estimates` and
  `covariance_matrix`. Ignored when `parameter_estimates` and
  `covariance_matrix` are given directly.

- plev:

  probability mass the THETA draws are truncated to, emitted as
  `$PRIOR NWPRI PLEV=`. NONMEM has no default that works with
  `TRUE=PRIOR` (it stops with
  `VALUE OF ARGUMENT 'PLEV' IS INAPPROPRIATE`), so this is always
  written out. The default is close enough to 1 that truncation is
  negligible.

- parameter_estimates:

  named numeric vector of point estimates, overriding
  `fit$parameter_estimates`.

- covariance_matrix:

  parameter uncertainty covariance matrix with parameter names as
  row/column names, overriding `fit$covariance_matrix`.

## Value

a Pharmpy NONMEM model object with the prior records added.

## Details

Combined with `$SIMULATION ... TRUE=PRIOR` (see
[`set_simulation_clean()`](https://insightrx.github.io/pharmr.extra/reference/set_simulation_clean.md))
this is the `uncertainty_engine = "nwpri"` route of
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md):
NONMEM draws a fresh parameter vector for every simulation subproblem,
so `n` uncertainty replicates cost one NONMEM compile rather than `n`.

## How the prior is parameterised

The prior is centred on the fit, so simulating from it reproduces the
fit's parameter uncertainty rather than adding information to it.

- `$THETAP` holds the THETA point estimates and `$THETAPV` the THETA
  block of the covariance matrix, i.e. a multivariate normal prior on
  THETA.

- `$OMEGAP` / `$SIGMAP` hold the OMEGA and SIGMA point estimates,
  mirroring the block structure of the model's own `$OMEGA` / `$SIGMA`
  records.

- `$OMEGAPD` / `$SIGMAPD` hold the inverse-Wishart degrees of freedom,
  one per block, chosen so the prior variance of each block matches the
  estimated standard error: `df = 2 * mean((estimate / se)^2) + p` over
  the block's `p` diagonal elements (Gisleskog, Karlsson and Beal 2002;
  the same rule PsN uses, which for the usual `p = 1` diagonal element
  reduces to `2 * (estimate / se)^2 + 1`).

Standard errors are taken as the square root of the covariance matrix
diagonal rather than from `fit$standard_errors`, so the emitted records
are internally consistent with the covariance matrix they came from.

Because NWPRI treats the THETA prior, each OMEGA block and each SIGMA
block as independent, the THETA-OMEGA and THETA-SIGMA covariances that
`$COVARIANCE` reports are *not* carried into the prior. See the
`uncertainty_engine` section of
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
for what that means in practice.

## Parameters not covered by the covariance matrix

Fixed parameters, and any other parameter the covariance matrix does not
cover, cannot be given an uncertainty. They still have to appear in the
prior records (NONMEM requires the prior to mirror the model's parameter
structure), so they are emitted with a negligible prior variance (a
relative standard deviation of `1e-3`) and are therefore held at their
point estimate for all practical purposes. A warning lists them.

Non-finite covariance elements — NONMEM reports `NaN` for parameters the
covariance step could not separate — are treated as zero, so such a
parameter takes the same route.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- run_nlme(model = model, data = data)
prior_model <- add_nwpri_prior(attr(fit, "final_model"), fit)
} # }
```
