# pharmr.extra (development version)

* `run_sim(n_uncertainty = )` on the sequential replicate path no longer dies
  in its progress bar under `Rscript` (#137). `cli` builds progress bars on
  top of its status-bar stack, and `call_nmfe()` called `cli_process_done()`
  even when `verbose = FALSE` had opened no status bar of its own — so every
  replicate closed `run_sim()`'s progress bar instead. `cli` then indexed the
  emptied stack and threw `subscript out of bounds` from the bar's deferred
  teardown, turning a completed simulation into an error and returning nothing
  to the caller. The unbalanced closes are fixed (`call_nmfe()`,
  `attach_fit_info()` for sim models, and a stray one in `call_psn()` that had
  no matching start at all), every remaining `cli_process_done()` now closes
  its own bar by id, and `run_sim()` drives its progress bar defensively so a
  stray close anywhere else can no longer fail a finished run.

* `call_nmfe(check_only = TRUE)` no longer leaks its status bar on the early
  return, and its `on.exit()` no longer discards `cli`'s own deferred cleanup.

* **Breaking-ish:** `run_sim(n_uncertainty = )` now defaults to
  `uncertainty_engine = "auto"`, which uses **NWPRI** wherever it applies —
  NONMEM, with `n_iterations = 1` — and `"replicates"` everywhere else (#134).
  Benchmarked against the previous default on a 1-cmt oral model with a
  700-row simulation dataset and `n_cores = 4`: 18.6 s -> 4.3 s at 50 draws,
  65.3 s -> 5.1 s at 200, and 339.2 s -> 9.2 s at 1000. The replicate loop pays
  a full NM-TRAN + compile + run per draw, so its cost is linear in
  `n_uncertainty`; NWPRI pays one compile per chunk and is close to flat.
  `n_cores` never helped the replicate loop on NONMEM in the first place (it
  warns and runs sequentially), so this is where the parallelism actually
  arrives for that backend.

  Naming an engine explicitly still errors rather than falling back, so an
  explicit `uncertainty_engine = "nwpri"` on an nlmixr2 run or with
  `n_iterations > 1` fails exactly as before. `"auto"` announces its choice
  under `verbose`.

  Two consequences worth knowing, both documented on `run_sim()`. NWPRI draws
  OMEGA/SIGMA from inverse-Wishart distributions and drops the THETA-OMEGA and
  THETA-SIGMA covariances `$COVARIANCE` reports, so draws are not distributed
  identically to the `"replicates"` engine's (aggregates agree to within a few
  percent; see `inst/reports/nwpri-validation.html`). And NWPRI cannot hold the
  simulated individuals fixed across draws the way `"replicates"` now does
  (#131), so an interval over `.uncertainty` also carries the Monte-Carlo noise
  of re-simulating the subjects — which shrinks as the dataset and
  `n_uncertainty` grow. Pass `uncertainty_engine = "replicates"` to get the old
  behaviour back.

* Parallel worker startup is no longer able to take down a whole run (#134).
  Bringing up the cluster happens before any work function is called, so it sat
  outside `run_captured()`'s error handling: an intermittent failure in a
  worker's `loadNamespace()` (seen inside rxode2's `.onLoad`) aborted the
  entire `run_sim()` call rather than costing one chunk. `parallel_lapply()`
  now retries once and then falls back to running everything sequentially with
  a warning. Same results either way, only slower.

* The NWPRI engine no longer splits the draws over more NONMEM jobs than the
  split is worth (#134). Each extra chunk costs a worker process to start and
  load the package (~1.4 s for four) against ~0.02 s per subproblem, so below
  ~50 draws per chunk the chunking made the run slower — measured at 2.6 s in
  one job versus 4.3 s over four for 50 draws. `n_cores` is now a ceiling
  rather than a target.

* `run_sim(n_uncertainty = )` with `uncertainty_engine = "replicates"` (the
  default) now simulates **every replicate with the same `seed`** instead of a
  per-replicate `seed + r` (#131). Both backends build ETAs and residuals by
  scaling standard normal deviates, so a shared seed means every draw sees the
  same underlying deviates and the only thing varying between replicates is
  the parameter vector — the common-random-numbers setup a confidence interval
  on a simulated percentile needs. Previously the spread across `.uncertainty`
  also contained the Monte-Carlo noise of re-simulating a fresh set of
  subjects for every draw, which inflated the interval. Use `n_iterations` if
  you want fresh random variability *within* a replicate. Sequential and
  parallel (`n_cores > 1`) runs are affected equally.

  This does **not** extend to `uncertainty_engine = "nwpri"`, and cannot:
  NONMEM continues its random sources from subproblem to subproblem and offers
  no option to rewind them, so each NWPRI draw necessarily re-simulates its
  own ETAs and residuals. NWPRI uncertainty intervals therefore still carry
  that noise; make the simulation dataset large enough that it is small, or
  use `"replicates"` when the separation matters. This is now documented on
  `run_sim()`.

* `run_sim()` gains a second parameter-uncertainty engine, selected with
  `uncertainty_engine = "nwpri"` (#130). Instead of drawing `n_uncertainty`
  parameter sets in R and running one NONMEM job per draw
  (`uncertainty_engine = "replicates"`, the unchanged default), it builds a
  `$PRIOR NWPRI` record from the fit and runs
  `$SIMULATION ... TRUE=PRIOR`, so NONMEM draws a new parameter vector for
  every subproblem. The whole set of draws then costs one NONMEM compile
  rather than `n_uncertainty` of them, which for short simulations is most of
  the run time. NONMEM only, and it requires `n_iterations = 1`, since every
  NWPRI subproblem redraws the parameters and so cannot repeat a draw.

  NONMEM will not parallelise this for us — MPI/`PARAFILE` splits the
  estimation and covariance steps, and a simulation-only model has neither, so
  a single `SUBPROBLEMS=N` run is single-threaded whatever `n_cores` says. The
  subproblems are therefore split over `n_cores` separate NONMEM jobs, one per
  worker process, each in its own run folder
  (`id/regimen_<i>/uncertainty_chunk_<k>`) with its own widely-spaced seed,
  and the tables are concatenated. Because the draws come out of NONMEM's RNG,
  *which* draws you get depends on how the subproblems were chunked, so an
  NWPRI run is only reproducible for a fixed `n_cores`. A chunk that fails is
  dropped with a warning, so a failure costs `n_uncertainty / n_cores` draws
  rather than one; as for the `"replicates"` engine, the result carries
  `n_uncertainty_requested` and `n_uncertainty_kept` attributes.

  The two engines are **not** statistically interchangeable, which is why this
  is a user-facing switch rather than a silent optimisation. NWPRI draws OMEGA
  and SIGMA from (right-skewed) inverse-Wishart distributions where
  `"replicates"` draws every parameter from one truncated multivariate normal,
  and NWPRI treats the THETA, OMEGA and SIGMA priors as independent blocks and
  so discards the THETA-OMEGA and THETA-SIGMA covariances that `$COVARIANCE`
  reports. Their first two moments still agree to within a few percent; see
  `inst/reports/nwpri-validation.html`.

* New `add_nwpri_prior()` builds the `$PRIOR NWPRI`, `$THETAP`, `$THETAPV`,
  `$OMEGAP`, `$OMEGAPD`, `$SIGMAP` and `$SIGMAPD` records from a fit and
  inserts them into a NONMEM model, mirroring the model's own `$OMEGA` /
  `$SIGMA` block structure and giving each block the inverse-Wishart degrees
  of freedom that match its estimated standard error. Parameters the
  covariance matrix does not cover (FIXED ones, typically) are emitted with a
  negligible prior variance and warned about, rather than dropped; so are
  parameters whose covariance NONMEM reports as `NaN` because the covariance
  step could not separate them. The
  generated control streams are checked against NONMEM 7.6.0: the fixture
  `tests/testthat/fixtures/nwpri_generated_anchor.rds` freezes, for a
  diagonal-OMEGA, a `BLOCK(2)`-OMEGA and a FIXED-THETA model, both the emitted
  stream and 1000 parameter vectors NONMEM drew from it, so the record
  construction is tested against NONMEM rather than against itself.

* `set_simulation_clean()` gains `true_prior`, and the new
  `set_simulation_record()` rewrites the `$SIMULATION` record of a control
  stream directly. The latter works on model code rather than on a Pharmpy
  model object because Pharmpy's `$SIMULATION` grammar rejects `TRUE=PRIOR`
  and refuses to parse such a model; `set_simulation_clean(true_prior = TRUE)`
  therefore returns the model code rather than a model object.

* `read_table_nm()` gains `subproblems`. A table written by a `$SIMULATION`
  record with `SUBPROBLEMS > 1` holds one block of rows per subproblem, each
  opened by a repeated `TABLE NO.` header; those headers were previously
  discarded along with the rest of the non-numeric rows, so the subproblem
  boundaries were lost. With `subproblems = TRUE` the table is split on them
  and a 1-based `.subproblem` column is added.

* `run_sim(n_uncertainty = )` can now run its replicates in parallel, via
  `n_cores` (#126). Replicates are independent — own parameter draw, own
  derived seed (`seed + r`), combined only at the end — so they are spread over
  PSOCK worker processes and reassembled by replicate index, which keeps the
  output identical to a sequential run for the same seed. Only the
  nlmixr2/rxode2 backend is parallelised: the NONMEM backend drives Pharmpy
  through Python, which cannot be sent to a worker, and writes per-regimen run
  folders that concurrent replicates would clobber; a NONMEM run warns and
  falls back to sequential. The machine's cores are divided over the workers,
  so a higher `n_cores` does not oversubscribe the CPU with rxode2 solver
  threads.

  Replicate failures are handled differently per backend. On nlmixr2 a failed
  replicate is dropped with a warning and the rest of the run continues; the
  result then carries `n_uncertainty_requested` and `n_uncertainty_kept`
  attributes, since replicates that fail tend to be the extreme parameter
  draws and a silently short set of draws would narrow any interval computed
  over `.uncertainty`. On NONMEM a failed replicate aborts the run, as before:
  those failures are typically systematic (licence, missing output table,
  clobbered run folder) rather than specific to one draw.

* The parameter-uncertainty sampling behind `run_sim(n_uncertainty = )` is now
  anchored against NONMEM's own uncertainty-simulation routine, `$PRIOR NWPRI`
  + `$SIMULATION ... TRUE=PRIOR`. Over 1000 draws from the same fit, means and
  standard deviations of the fixed effects agree to within 0.3% and 2.4%, those
  of the variance parameters to within 4.4% and 7.3%, and the resulting 90%
  uncertainty interval on the predicted profile to within 6.3%. The two
  remaining differences are structural and are asserted explicitly by the tests
  rather than absorbed into loose tolerances: NWPRI draws OMEGA and SIGMA from
  right-skewed inverse-Wishart distributions where we use a single truncated
  multivariate normal, and NWPRI treats the THETA, OMEGA and SIGMA priors as
  independent blocks, discarding the THETA-OMEGA and THETA-SIGMA covariances
  that `$COVARIANCE` reports and that our draws keep.

  The NONMEM side is generated once and frozen as a fixture
  (`tests/testthat/fixtures/_create-nwpri-anchor.R` regenerates it inside the
  `pmx` container), so `tests/testthat/test-run_sim-nwpri.R` needs Pharmpy but
  not NONMEM and runs in CI. A write-up of the comparison, including why we
  sample from the covariance matrix ourselves instead of delegating to NWPRI,
  is in `inst/reports/nwpri-validation.html`.

* `run_nlme()` fits now return a `residuals` element that can be joined to the
  dataset (#120). Pharmpy returns residuals indexed by dataset row label and
  drops every row whose residual columns are all exactly 0 — reticulate then
  drops the index on conversion, so `fit$residuals` reached R with neither a
  join key nor a row count matching the observation records (1134 vs 2184 in
  the admiral popPK example). `residuals` is now rebuilt from the run's output
  tables with one row per observation record — the rows of `fit$predictions`
  for which `model$dataset$MDV == 0` — plus `ROW` (row number in
  `model$dataset`) and the model's ID and independent-variable columns (`ID`
  and `TIME` for a typical NONMEM dataset; the names are taken from the
  model's datainfo) as join keys, and all residual columns written to the
  tables (`CWRES`, `CIWRES`, `NPDE`, ...). So

  ``` r
  dplyr::left_join(
    dplyr::mutate(model$dataset, ROW = dplyr::row_number()),
    fit$residuals,
    by = "ROW"
  )
  ```

  attaches the residuals to the dataset. Rows NONMEM reported as 0 are kept.
  For NONMEM fits the pandas index is still set to the `model$dataset` row
  labels, so Pharmpy tools that join on it (plots, `ruvsearch`) are
  unaffected. nlmixr2 fits get the same shape, keyed against the data they
  were actually fitted to.

* `call_pharmpy_tool()` now works for `bootstrap` and `modelsearch` (and the
  other search tools) against **nlmixr2** models (#121). Pharmpy's nlmixr
  backend had three bugs that made every candidate fit fail, so those
  workflows aborted before returning results:
  1. `IndexingError: Too many indexers` — `parse_modelfit_results()` indexes
     the thetas table with `get_thetas(model).names`, which is a tuple in
     Pharmpy >= 2.0, and pandas reads a tuple passed to `.loc` as a
     multi-axis indexer.
  2. `ValueError: Length mismatch` — the same function indexes the
     predictions by `DV != 0`, but nlmixr2 returns one row per *observation
     record*, so any dataset with a zero-valued observation (BLQ imputed to
     0, a baseline sample) has too few index labels.
  3. `cannot open file '.../<model>.csv'` — `execute_model()` writes the
     candidate's dataset under its datainfo name but generates an R script
     that reads `<model name>.csv`; every `modelsearch` candidate inherits a
     datainfo path from the input model, so no candidate ever ran.
  These are patched in the Python session by the new
  `patch_pharmpy_nlmixr_results()`, which `call_pharmpy_tool()` applies
  automatically (best-effort) for nlmixr-format models. The patch covers both
  the `pharmpy.tools.external.nlmixr.run` module and the package-level
  `parse_modelfit_results` alias, so `pharmpy.tools.read_modelfit_results()`
  and `bootstrap`'s results parsing are fixed as well. It is idempotent and is
  a no-op on a Pharmpy release that has fixed them.

* nlmixr2 fits converted to Pharmpy-native `ModelfitResults` now carry an
  empty `Log` instead of `None` (#121). Pharmpy tools summarize errors across
  model entries with `len(res.log)` and no `None` check, so `modelsearch`
  failed in post-processing with
  `TypeError: object of type 'NoneType' has no len()`.

* `call_pharmpy_tool()` no longer discards a completed search when the final
  model's estimates cannot be written back as initial estimates (#121). A
  degenerate candidate fit can return an estimate outside its parameter's
  bounds — nlmixr2 reports the unconstrained optimum, e.g. a negative
  `POP_CL` for a structurally wrong candidate — and Pharmpy rejects that with
  `ValueError: Lower bound 0.0 cannot be greater than init`, which used to
  abort `modelsearch` *after* the search itself had finished. The tool result
  is now returned with a warning, with the final model keeping its original
  initial estimates.

* `create_run_folder()` no longer errors with `argument is of length zero`
  when `force` is `NULL` — the default that `run_nlme()` passes down. An
  unspecified `force` now means "do not overwrite", so re-running into an
  existing run folder gives the intended
  `Run folder (...) exists. Use \`force\` to overwrite.` message. Previously
  this made every second nlmixr2 `run_nlme()` into the same `id` fail with an
  unrelated error. Non-logical truthy values (`1`, `"TRUE"`, e.g. round-tripped
  through JSON or a CLI) keep their previous meaning and still overwrite.


* `$TABLE` records written by `add_table_to_model()`, `add_default_output_tables()`,
  `run_sim()` and `create_vpc_data()` no longer round every output column to a
  whole number (#114, a regression in 0.0.0.9092). Those functions widened the
  `ID` column with `FORMAT=sF9.0`, but NONMEM applies `FORMAT` to *all* columns
  of the table — and to all subsequent `$TABLE` records — so concentrations,
  times and parameter columns were quantised (severely distorting VPC data).
  The ID column is now widened with `IDFORMAT=sF11.0`, which formats the `ID`
  column only (integer IDs up to 10 digits) and leaves every other column at
  NONMEM's default precision. A table-wide `FORMAT` is still available as an
  opt-in `format` argument of `add_table_to_model()`, and `create_vpc_data()`
  gained an `id_format` argument.

* `run_nlme()` SAEM fits of nlmixr2 models no longer fail when the model was
  modified by a pharmpy operation after `create_model()` (e.g.
  `create_model(...) |> set_initial_estimates(...)`). Such operations return a
  new pharmpy object that drops the SAEM-safe `nlmixr_code` attribute cached by
  `create_model()`; `run_nlme_nlmixr()` now reapplies the residual-alias rewrite
  to the fallback `model$code` so SAEM accepts the residual error formula.

* `create_model(tool = "nlmixr2")` no longer aborts with
  `ValueError: datainfo.path is None` when `data` is supplied as a data.frame.
  `clean_modelfit_data()` was calling `pharmr::load_dataset()` after
  `set_dataset()`; the reload was redundant (the dataset is already attached)
  and failed for nlmixr2 models, whose datainfo has no on-disk path.

* `create_model()` now writes a data.frame `data` argument to a CSV in the
  session tempdir and points the model's `$DATA` record at that file, instead
  of leaving pharmpy's `DUMMYPATH` placeholder. This gives the model an on-disk
  dataset, which is required to use the `run_nlme(copy_dataset = FALSE)`
  workflow (NONMEM models only; filename input already has an on-disk dataset).

* `copy_dataset = FALSE` can only be honored when the dataset is a file on
  disk (supplied via `data` or referenced by the model's `$DATA` record). When
  only an in-memory dataset is available (a passed data frame, `model$dataset`,
  or the original dataset), a warning is issued and the dataset is copied into
  the run folder (with `$DATA` rewritten) as a fallback.
* `copy_dataset = FALSE` now leaves the model's `$DATA` record untouched
  instead of rewriting it to the dataset's absolute path. Combined with not
  copying the dataset into the run folder, the model's original data reference
  is preserved verbatim. (`$DATA` is still rewritten when `copy_dataset = TRUE`,
  i.e. when the dataset is placed into the run folder as `data.csv`.)
* `run_nlme(data = NULL, copy_dataset = FALSE)` now correctly leaves the
  dataset in its existing location when the model's `$DATA` record points to a
  real file. Previously the dataset was always copied into the run folder
  because `run_nlme()` materialised `model$dataset` into a tempfile before
  reaching `prepare_run_folder()`, and `prepare_run_folder()` preferred the
  in-memory dataset over the on-disk $DATA path.
* `update_parameters()` now also accepts a raw `nlmixr2FitCore` / `nlmixr2FitData`
  object — useful when fitting outside `run_nlme()`. Both diagonal and
  off-diagonal omega elements are extracted and named per pharmpy's
  `IIV_X` / `IIV_X_IIV_Y` convention, so block-omega covariances are now
  updated alongside variance terms (previously dropped).
* `run_nlme()`, `run_sim()`, and `create_vpc_data()` now dispatch on the model
  engine. Pharmpy nlmixr-format models are routed through nlmixr2 / rxode2
  directly (no pharmpy `pyreadr` dependency). NONMEM models still use the
  existing nmfe / PsN / pharmpy paths unchanged.
* New `control` argument on `run_nlme()` is forwarded to
  `nlmixr2::nlmixr2()` (e.g. `foceiControl()`); ignored for NONMEM.
* `validate_model()` now accepts nlmixr-format pharmpy models.
* `get_advan()` / `get_obs_compartment()` return gracefully for non-NONMEM
  models instead of erroring on the missing control stream.
* `add_sir()` now warns and no-ops for non-NONMEM models instead of silently
  doing nothing or aborting deep inside the covariance-record reader.
* `call_pharmpy_tool()` now forwards `esttool = "nlmixr"` to pharmpy when a
  search tool (`modelsearch`, `covsearch`, `iivsearch`, `ruvsearch`, `amd`,
  `bootstrap`) is invoked with an nlmixr-format model. Pharmpy can drive
  these searches against nlmixr2 if the Python package `pyreadr` is
  installed and the system Rscript that pharmpy spawns has a working
  nlmixr2 / data.table install.
* `compare_nlme_runs()` now detects the engine per run folder and loads
  nlmixr2 fits from the saved `<id>.rds` next to the run directory, so
  `compare_nlme_runs()` works for nlmixr2 runs too.
* `create_modelfit_info_table()` is robust to fit objects reloaded from RDS
  whose pharmpy model attribute is no longer a live Python reference.
* `create_vpc_data()` on an nlmixr fit now uses the fitted model's
  parameter estimates (was silently falling back to the pre-fit initial
  estimates when only a `fit` was supplied), and restricts `obs` to
  observation rows so it matches the simulation row set.
* `run_nlme_nlmixr()` now stashes an explicit `data` argument on the
  model so saved fits sim/VPC against the right dataset (previously the
  saved model still referenced the original `model$dataset`).

* Initial CRAN submission.
