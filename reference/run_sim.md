# Run simulations

Run simulations

## Usage

``` r
run_sim(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = irxutils::get_random_id("sim_"),
  path = NULL,
  force = FALSE,
  tool = c("auto", "nonmem", "nlmixr2"),
  n_iterations = 1,
  n_uncertainty = NULL,
  variables = NULL,
  add_pk_variables = FALSE,
  output_file = "simtab",
  update_table = TRUE,
  seed = 12345,
  verbose = TRUE,
  n_cores = 1,
  uncertainty_engine = c("auto", "replicates", "nwpri"),
  plev = 0.9999
)
```

## Arguments

- fit:

  a Pharmpy modelfit object.

- data:

  a NONMEM-format data.frame to use as the simulation dataset. Typically
  the output of
  [`create_sim_dataset()`](https://insightrx.github.io/pharmr.extra/reference/create_sim_dataset.md).
  If `NULL`, the dataset attached to `model` is used as-is.

- model:

  either a Pharmpy model object, or a filename (for a model with NONMEM
  model code). If the latter, `run_sim()` will attempt to load the model
  into Pharmpy first.

- id:

  base run id (default a random `sim_*`). NONMEM only: nlmixr2
  simulations are solved in memory and write no run folders, so `id`
  does nothing there. Each regimen is run in its own subfolder
  `id/regimen_<i>` (`<i>` = 1-based regimen index), so regimens don't
  overwrite each other's output. Under
  `uncertainty_engine = "replicates"` each draw gets a folder of its own
  too, `id/uncertainty_<r>/regimen_<i>` (`<r>` = 1-based replicate
  index), so every replicate's NONMEM artifacts can be inspected
  afterwards and concurrent replicates cannot clobber each other.

- path:

  folder in which to create the run folder(s). Each regimen is run in
  its own subfolder `id/regimen_<i>` (see `id` for the uncertainty
  layout). If `NULL` (default), the folder is forwarded to
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  unset, so
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)'s
  own default applies.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- tool:

  the tool to run the model in, either `nonmem`, or `nlmixr`.

- n_iterations:

  number of iterations of the entire simulation to perform. The dataset
  for the simulation will stay the same between each iterations.

- n_uncertainty:

  number of parameter sets to draw from the fit's covariance matrix to
  propagate parameter uncertainty. If `NULL` (default) or `0`, the point
  estimates are used and no uncertainty is propagated. If a positive
  integer, the point estimate is omitted and `n_uncertainty` parameter
  sets are sampled instead; one simulation is run per draw with its
  thetas/omegas/sigmas updated, so a total of
  `n_iterations * n_uncertainty` simulations are performed. Requires a
  `fit` object carrying a covariance matrix (i.e. the model was run with
  a `$COVARIANCE` step or SIR). When set, the output gains a
  `.uncertainty` column counting the replicate (1-based).

  Only parameters present in the covariance matrix are resampled; any
  other estimated parameters are held at their point estimates and a
  warning lists them. This matters for nlmixr2 fits in particular: the
  default nlmixr2 covariance step reports uncertainty only for the
  population fixed effects, so residual and random-effect variance
  parameters (SIGMA, OMEGA/IIV) are held fixed. For full uncertainty on
  those, use a bootstrap (`nlmixr2est::bootstrapFit()`). NONMEM
  `$COVARIANCE` typically covers all parameters, so all are resampled.

  Every replicate is simulated with the **same** `seed` (common random
  numbers), so the sequence of standard normal deviates behind the
  simulated ETAs and residuals is identical across draws and the only
  thing that varies between replicates is the parameter vector. This is
  what makes a percentile computed per replicate a clean estimate of
  parameter uncertainty; with a different seed per replicate the spread
  across replicates would also contain the Monte-Carlo noise of
  re-simulating a fresh set of subjects each time. Use `n_iterations` if
  you want extra random variability *within* a replicate. Note this
  holds for `uncertainty_engine = "replicates"` only — see
  `uncertainty_engine` below for why NWPRI cannot do it, and why it is
  nonetheless the default.

  This is the same idea as NONMEM's own `$PRIOR NWPRI` +
  `$SIMULATION ... TRUE=PRIOR`, which is available directly as
  `uncertainty_engine = "nwpri"` (see below). The two are checked
  against each other in `tests/testthat/test-run_sim-nwpri.R`.
  Aggregates agree closely: over 1000 draws from the same fit, means and
  standard deviations of the fixed effects match to within 0.3% and 3%,
  those of the variance parameters to within 5% and 8%, and the
  resulting 90% uncertainty interval on the predicted profile to within
  7%. Two differences are structural rather than numerical: NWPRI draws
  OMEGA and SIGMA from (right-skewed) inverse-Wishart distributions
  whereas the draws here come from a single truncated multivariate
  normal, and NWPRI treats the THETA, OMEGA and SIGMA priors as
  independent blocks whereas the draws here keep the THETA-OMEGA and
  THETA-SIGMA covariances that `$COVARIANCE` reports.

- variables:

  vector of variables to output. If `NULL`, will output default
  variables `c("ID", "TIME", "DV", "EVID", "PRED")` as well as all
  variables declared in the NONMEM code.

- add_pk_variables:

  calculate basic PK variables: CMAX_OBS, TMAX_OBS, CMIN_OBS, and (when
  `CL` is in the output table) AUC_SS. AUC_SS is derived as the last
  dose in the simulation dataset divided by CL.

- output_file:

  TODO

- update_table:

  should any existing \$TABLE records be removed, and a new `simtab` be
  created? This is default. If `FALSE`, it will leave \$TABLEs as
  specifed in the model. However, in the return object, only the first
  table is returned back. If `FALSE`, the `add_pk_variables` argument
  will be ignored.

- seed:

  TODO

- verbose:

  verbose output?

- n_cores:

  number of processes to run uncertainty replicates on (default `1`,
  i.e. sequential; unchanged behaviour). Values `> 1` spread the
  `n_uncertainty` replicates over that many worker processes. For
  `uncertainty_engine = "replicates"` both backends are parallelised:
  the replicates are prepared in this process (applying the draw needs
  Pharmpy for NONMEM, rxode2 code generation for nlmixr2) and the
  workers only run the simulation. Output is identical to a sequential
  run for the same `seed`, since every replicate is run with the same
  `seed` and results are reassembled by replicate index. The unit of
  work is the replicate, not the regimen, so more workers than
  `n_uncertainty` buys nothing. For `uncertainty_engine = "nwpri"`
  (NONMEM only) it sets how many NONMEM jobs the subproblems are split
  over, one per worker process. NONMEM's own RNG produces the draws, so
  *which* draws you get depends on how the subproblems were chunked: an
  NWPRI run is only reproducible for a fixed `n_cores`. Note also that a
  chunk that fails costs `n_uncertainty / n_cores` draws rather than
  one. Ignored when no uncertainty is requested. The machine's cores are
  divided over the workers (rxode2's solver threads are capped per
  worker), so raising `n_cores` does not oversubscribe the CPU.

- uncertainty_engine:

  how `n_uncertainty` parameter uncertainty is propagated. Ignored when
  no uncertainty is requested.

  - `"auto"` (default) uses `"nwpri"` where it applies — NONMEM, with
    `n_iterations = 1` — and `"replicates"` everywhere else. Naming an
    engine explicitly errors rather than falling back, so an explicit
    request is never silently overridden; `"auto"` announces which one
    it picked under `verbose`.

  - `"replicates"` draws `n_uncertainty` parameter sets from the fit's
    covariance matrix in R and runs one simulation per draw. Works for
    both backends.

  - `"nwpri"` (NONMEM only) hands the job to NONMEM: a `$PRIOR NWPRI`
    record built from the fit (see
    [`add_nwpri_prior()`](https://insightrx.github.io/pharmr.extra/reference/add_nwpri_prior.md))
    plus `$SIMULATION ... TRUE=PRIOR`, so NONMEM draws a new parameter
    vector per subproblem. That costs one NONMEM compile for the whole
    set instead of one per draw, which for short simulations dominates
    the run time, so it is much faster for large `n_uncertainty`. It
    requires `n_iterations = 1`, because every NWPRI subproblem redraws
    the parameters and so cannot repeat a draw.

  `"nwpri"` cannot give you common random numbers across draws. NONMEM
  continues its random sources from subproblem to subproblem and offers
  no way to rewind them, so each subproblem simulates a *different* set
  of ETAs and residuals in addition to a different parameter vector.
  Uncertainty intervals computed over `.uncertainty` from an NWPRI run
  therefore also contain the Monte-Carlo noise of re-simulating the
  subjects; make the simulation dataset large enough that this noise is
  small, or use `"replicates"` when a clean separation matters.

  The two are **not** statistically interchangeable. Over 1000 draws
  from the same fit their means and standard deviations agree to within
  a few percent (see `inst/reports/nwpri-validation.html`), but two
  differences are structural rather than numerical: NWPRI draws OMEGA
  and SIGMA from (right-skewed) inverse-Wishart distributions where
  `"replicates"` draws every parameter from one truncated multivariate
  normal, and NWPRI treats the THETA, OMEGA and SIGMA priors as
  independent blocks and therefore discards the THETA-OMEGA and
  THETA-SIGMA covariances that `$COVARIANCE` reports. Which is
  preferable is a judgement call — the inverse-Wishart draw is arguably
  better justified for variance parameters, joint sampling is the one
  that keeps the full reported covariance — which is why this stays a
  switch rather than becoming an implementation detail.

  A third difference matters for uncertainty intervals specifically:
  NWPRI cannot hold the simulated individuals fixed across draws, where
  `"replicates"` does (see `n_uncertainty` above, and issue \#131). An
  NWPRI interval over `.uncertainty` therefore also carries the
  Monte-Carlo noise of re-simulating the subjects. That noise shrinks as
  the simulation dataset and `n_uncertainty` grow, which is the regime
  the speed difference makes practical, so NWPRI is nonetheless the
  default. Use `uncertainty_engine = "replicates"` when a clean
  separation matters more than run time — small simulation datasets and
  few draws being the case to watch.

- plev:

  `uncertainty_engine = "nwpri"` only: the probability mass the THETA
  draws are truncated to, passed to
  [`add_nwpri_prior()`](https://insightrx.github.io/pharmr.extra/reference/add_nwpri_prior.md).

## Value

data.frame with simulation results. When `n_uncertainty` is used, the
result also carries `n_uncertainty_requested` and `n_uncertainty_kept`
attributes: replicates that fail on the nlmixr2 backend are dropped with
a warning, so these let a caller detect a short (and potentially biased)
set of draws without parsing warnings. On the NONMEM backend a failing
replicate aborts the run instead. Under `uncertainty_engine = "nwpri"` a
failing *chunk* is dropped with a warning rather than aborting, and the
same two attributes report how many draws survived — counted per regimen
and reported for the worst one, since chunks are per regimen and the
draws only pair across regimens where every regimen kept them.
