# SIR covariance estimation in NONMEM

## What is SIR?

Sampling Importance Resampling (SIR) is a method for estimating
parameter uncertainty that NONMEM (7.3+) can run as part of the
`$COVARIANCE` step. It works by:

1.  **Sampling** parameter vectors from a proposal distribution
    (typically the asymptotic normal distribution centered on the FOCE
    estimates, with the covariance matrix as the variance).
2.  **Weighting** each sample by how well it matches the observed data
    relative to the proposal distribution.
3.  **Resampling** with replacement according to those weights, yielding
    a set of parameter draws that approximate the true posterior.

The output is a set of weighted parameter vectors rather than a single
covariance matrix, which makes SIR more robust than the standard
covariance step when:

- The dataset is small and the asymptotic covariance matrix is
  unreliable.
- The model is highly nonlinear or the parameter distributions are
  skewed.
- The standard covariance step fails (e.g., non-positive-definite R
  matrix).
- A full multivariate uncertainty distribution is needed for simulation.

SIR is slower than the standard covariance step but much faster than a
nonparametric bootstrap, and it produces uncertainty estimates that
closely match likelihood profiling in practice (Dosne et al., 2016,
<doi:10.1007/s10928-016-9487-8>).

## NONMEM control stream

Add SIR to the `$COVARIANCE` record with `SIRSAMPLE` (number of proposal
samples, *M*) and `SIRNITER` (number of SIR iterations, *m*):

    $COVARIANCE UNCONDITIONAL SIRSAMPLE=1000 SIRNITER=1

Typical settings:

| Setting | Value | Notes |
|----|----|----|
| `SIRSAMPLE` | 500–5000 | More samples → better coverage but longer runtime |
| `SIRNITER` | 1–10 | Additional iterations refine the proposal; 1 is usually sufficient |

A ratio of `SIRSAMPLE / SIRNITER` ≥ 5 is generally recommended. Start
with `SIRSAMPLE=1000 SIRNITER=1` and increase `SIRSAMPLE` if diagnostic
plots show proposal depletion (see below).

## Setting up SIR with pharmr.extra

Use
[`add_sir()`](https://insightrx.github.io/pharmr.extra/reference/add_sir.md)
to add SIR options to an existing Pharmpy model object:

``` r

library(pharmr.extra)

model <- pharmr::read_model("run1/run.mod")
model <- add_sir(model, options = list(samples = 1000, niter = 1))
```

[`add_sir()`](https://insightrx.github.io/pharmr.extra/reference/add_sir.md)
appends `SIRSAMPLE=` and `SIRNITER=` to the existing `$COVARIANCE`
record. The model must already have a `$COVARIANCE` record; use
[`set_covariance()`](https://insightrx.github.io/pharmr.extra/reference/set_covariance.md)
to add one first if needed.

## Reading results

When a model is run with `SIRSAMPLE`, NONMEM writes an additional table
to the `.ext` file for each SIR iteration. Use
[`pharmr.extra::read_modelfit_results()`](https://insightrx.github.io/pharmr.extra/reference/read_modelfit_results.md)
instead of
[`pharmr::read_modelfit_results()`](https://rdrr.io/pkg/pharmr/man/read_modelfit_results.html)
— the former applies patches for known Pharmpy bugs that cause all-NaN
parameter estimates and a missing covariance matrix when a SIR table is
present:

``` r

fit <- read_modelfit_results("run1/run.mod")

fit$parameter_estimates   # FOCE point estimates (from table 1)
fit$standard_errors       # SIR-based SEs (from last SIR iteration table)
fit$covariance_matrix     # covariance matrix from the .cov file
fit$covstep_successful    # TRUE if the covariance step completed
```

The
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
workflow calls
[`read_modelfit_results()`](https://insightrx.github.io/pharmr.extra/reference/read_modelfit_results.md)
automatically, so no extra steps are needed when using the standard
fitting pipeline.

## Diagnostic checks

Three plots in the lst-file output help assess SIR quality:

**dOFV distribution** — the distribution of
`dOFV = OFV(sample) - OFV(FOCE)` values for the SIR samples should
follow a chi-squared distribution. If the distribution is shifted to the
right, the proposal is too narrow; increase `SIRSAMPLE`.

**Spatial trends** — a scatter plot of importance ratios vs. individual
parameters. A flat (horizontal) trend indicates a good proposal match. A
bell-shaped trend means the proposal is too wide; a U-shaped trend means
it is too narrow.

**Temporal trends** — importance ratio vs. sample index. A flat trend
indicates that good samples are not being depleted. If the trend drifts
down over iterations, increase `SIRSAMPLE` or `SIRNITER`.

## Example workflow

``` r

library(pharmr.extra)

# Build model, add covariance step and SIR
model <- create_model(
  data        = pk_data,
  compartments = 2,
  oral        = TRUE,
  ruv         = list(proportional = TRUE)
)
model <- set_covariance(model)
model <- add_sir(model, options = list(samples = 1000, niter = 1))

# Fit with NONMEM
result <- run_nlme(model, run_folder = "run1")

# Read results (SIR-aware)
fit <- read_modelfit_results("run1/run.mod")
fit$parameter_estimates
fit$standard_errors
fit$covstep_successful
```

## References

Dosne A-G, Bergstrand M, Karlsson MO. An automated sampling importance
resampling procedure for estimating parameter uncertainty. *J
Pharmacokinet Pharmacodyn.* 2017;44(6):509–520.
<doi:10.1007/s10928-016-9487-8>
