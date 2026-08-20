# Changelog

## pharmr.extra (development version)

- The parameter-uncertainty sampling behind `run_sim(n_uncertainty = )`
  is now anchored against NONMEM’s own uncertainty-simulation routine,
  `$PRIOR NWPRI`

  - `$SIMULATION ... TRUE=PRIOR`. Over 1000 draws from the same fit,
    means and standard deviations of the fixed effects agree to within
    0.3% and 2.4%, those of the variance parameters to within 4.4% and
    7.3%, and the resulting 90% uncertainty interval on the predicted
    profile to within 6.3%. The two remaining differences are structural
    and are asserted explicitly by the tests rather than absorbed into
    loose tolerances: NWPRI draws OMEGA and SIGMA from right-skewed
    inverse-Wishart distributions where we use a single truncated
    multivariate normal, and NWPRI treats the THETA, OMEGA and SIGMA
    priors as independent blocks, discarding the THETA-OMEGA and
    THETA-SIGMA covariances that `$COVARIANCE` reports and that our
    draws keep.

  The NONMEM side is generated once and frozen as a fixture
  (`tests/testthat/fixtures/_create-nwpri-anchor.R` regenerates it
  inside the `pmx` container), so `tests/testthat/test-run_sim-nwpri.R`
  needs Pharmpy but not NONMEM and runs in CI. A write-up of the
  comparison, including why we sample from the covariance matrix
  ourselves instead of delegating to NWPRI, is in
  `inst/reports/nwpri-validation.html`.

- [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  fits now return a `residuals` element that can be joined to the
  dataset (#120). Pharmpy returns residuals indexed by dataset row label
  and drops every row whose residual columns are all exactly 0 —
  reticulate then drops the index on conversion, so `fit$residuals`
  reached R with neither a join key nor a row count matching the
  observation records (1134 vs 2184 in the admiral popPK example).
  `residuals` is now rebuilt from the run’s output tables with one row
  per observation record — the rows of `fit$predictions` for which
  `model$dataset$MDV == 0` — plus `ROW` (row number in `model$dataset`)
  and the model’s ID and independent-variable columns (`ID` and `TIME`
  for a typical NONMEM dataset; the names are taken from the model’s
  datainfo) as join keys, and all residual columns written to the tables
  (`CWRES`, `CIWRES`, `NPDE`, …). So

  ``` r

  dplyr::left_join(
    dplyr::mutate(model$dataset, ROW = dplyr::row_number()),
    fit$residuals,
    by = "ROW"
  )
  ```

  attaches the residuals to the dataset. Rows NONMEM reported as 0 are
  kept. For NONMEM fits the pandas index is still set to the
  `model$dataset` row labels, so Pharmpy tools that join on it (plots,
  `ruvsearch`) are unaffected. nlmixr2 fits get the same shape, keyed
  against the data they were actually fitted to.

- [`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
  now works for `bootstrap` and `modelsearch` (and the other search
  tools) against **nlmixr2** models (#121). Pharmpy’s nlmixr backend had
  three bugs that made every candidate fit fail, so those workflows
  aborted before returning results:

  1.  `IndexingError: Too many indexers` — `parse_modelfit_results()`
      indexes the thetas table with `get_thetas(model).names`, which is
      a tuple in Pharmpy \>= 2.0, and pandas reads a tuple passed to
      `.loc` as a multi-axis indexer.
  2.  `ValueError: Length mismatch` — the same function indexes the
      predictions by `DV != 0`, but nlmixr2 returns one row per
      *observation record*, so any dataset with a zero-valued
      observation (BLQ imputed to 0, a baseline sample) has too few
      index labels.
  3.  `cannot open file '.../<model>.csv'` — `execute_model()` writes
      the candidate’s dataset under its datainfo name but generates an R
      script that reads `<model name>.csv`; every `modelsearch`
      candidate inherits a datainfo path from the input model, so no
      candidate ever ran. These are patched in the Python session by the
      new
      [`patch_pharmpy_nlmixr_results()`](https://insightrx.github.io/pharmr.extra/reference/patch_pharmpy_nlmixr_results.md),
      which
      [`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
      applies automatically (best-effort) for nlmixr-format models. The
      patch covers both the `pharmpy.tools.external.nlmixr.run` module
      and the package-level `parse_modelfit_results` alias, so
      `pharmpy.tools.read_modelfit_results()` and `bootstrap`’s results
      parsing are fixed as well. It is idempotent and is a no-op on a
      Pharmpy release that has fixed them.

- nlmixr2 fits converted to Pharmpy-native `ModelfitResults` now carry
  an empty `Log` instead of `None` (#121). Pharmpy tools summarize
  errors across model entries with `len(res.log)` and no `None` check,
  so `modelsearch` failed in post-processing with
  `TypeError: object of type 'NoneType' has no len()`.

- [`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
  no longer discards a completed search when the final model’s estimates
  cannot be written back as initial estimates (#121). A degenerate
  candidate fit can return an estimate outside its parameter’s bounds —
  nlmixr2 reports the unconstrained optimum, e.g. a negative `POP_CL`
  for a structurally wrong candidate — and Pharmpy rejects that with
  `ValueError: Lower bound 0.0 cannot be greater than init`, which used
  to abort `modelsearch` *after* the search itself had finished. The
  tool result is now returned with a warning, with the final model
  keeping its original initial estimates.

- [`create_run_folder()`](https://insightrx.github.io/pharmr.extra/reference/create_run_folder.md)
  no longer errors with `argument is of length zero` when `force` is
  `NULL` — the default that
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  passes down. An unspecified `force` now means “do not overwrite”, so
  re-running into an existing run folder gives the intended
  `Run folder (...) exists. Use \`force\` to
  overwrite.`message. Previously this made every second nlmixr2`run_nlme()`into the same`id`fail with an unrelated error. Non-logical truthy values (`1`,`“TRUE”\`,
  e.g. round-tripped through JSON or a CLI) keep their previous meaning
  and still overwrite.

- `$TABLE` records written by
  [`add_table_to_model()`](https://insightrx.github.io/pharmr.extra/reference/add_table_to_model.md),
  [`add_default_output_tables()`](https://insightrx.github.io/pharmr.extra/reference/add_default_output_tables.md),
  [`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
  and
  [`create_vpc_data()`](https://insightrx.github.io/pharmr.extra/reference/create_vpc_data.md)
  no longer round every output column to a whole number (#114, a
  regression in 0.0.0.9092). Those functions widened the `ID` column
  with `FORMAT=sF9.0`, but NONMEM applies `FORMAT` to *all* columns of
  the table — and to all subsequent `$TABLE` records — so
  concentrations, times and parameter columns were quantised (severely
  distorting VPC data). The ID column is now widened with
  `IDFORMAT=sF11.0`, which formats the `ID` column only (integer IDs up
  to 10 digits) and leaves every other column at NONMEM’s default
  precision. A table-wide `FORMAT` is still available as an opt-in
  `format` argument of
  [`add_table_to_model()`](https://insightrx.github.io/pharmr.extra/reference/add_table_to_model.md),
  and
  [`create_vpc_data()`](https://insightrx.github.io/pharmr.extra/reference/create_vpc_data.md)
  gained an `id_format` argument.

- [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  SAEM fits of nlmixr2 models no longer fail when the model was modified
  by a pharmpy operation after
  [`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md)
  (e.g. `create_model(...) |> set_initial_estimates(...)`). Such
  operations return a new pharmpy object that drops the SAEM-safe
  `nlmixr_code` attribute cached by
  [`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md);
  [`run_nlme_nlmixr()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme_nlmixr.md)
  now reapplies the residual-alias rewrite to the fallback `model$code`
  so SAEM accepts the residual error formula.

- `create_model(tool = "nlmixr2")` no longer aborts with
  `ValueError: datainfo.path is None` when `data` is supplied as a
  data.frame.
  [`clean_modelfit_data()`](https://insightrx.github.io/pharmr.extra/reference/clean_modelfit_data.md)
  was calling
  [`pharmr::load_dataset()`](https://rdrr.io/pkg/pharmr/man/load_dataset.html)
  after `set_dataset()`; the reload was redundant (the dataset is
  already attached) and failed for nlmixr2 models, whose datainfo has no
  on-disk path.

- [`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md)
  now writes a data.frame `data` argument to a CSV in the session
  tempdir and points the model’s `$DATA` record at that file, instead of
  leaving pharmpy’s `DUMMYPATH` placeholder. This gives the model an
  on-disk dataset, which is required to use the
  `run_nlme(copy_dataset = FALSE)` workflow (NONMEM models only;
  filename input already has an on-disk dataset).

- `copy_dataset = FALSE` can only be honored when the dataset is a file
  on disk (supplied via `data` or referenced by the model’s `$DATA`
  record). When only an in-memory dataset is available (a passed data
  frame, `model$dataset`, or the original dataset), a warning is issued
  and the dataset is copied into the run folder (with `$DATA` rewritten)
  as a fallback.

- `copy_dataset = FALSE` now leaves the model’s `$DATA` record untouched
  instead of rewriting it to the dataset’s absolute path. Combined with
  not copying the dataset into the run folder, the model’s original data
  reference is preserved verbatim. (`$DATA` is still rewritten when
  `copy_dataset = TRUE`, i.e. when the dataset is placed into the run
  folder as `data.csv`.)

- `run_nlme(data = NULL, copy_dataset = FALSE)` now correctly leaves the
  dataset in its existing location when the model’s `$DATA` record
  points to a real file. Previously the dataset was always copied into
  the run folder because
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  materialised `model$dataset` into a tempfile before reaching
  [`prepare_run_folder()`](https://insightrx.github.io/pharmr.extra/reference/prepare_run_folder.md),
  and
  [`prepare_run_folder()`](https://insightrx.github.io/pharmr.extra/reference/prepare_run_folder.md)
  preferred the in-memory dataset over the on-disk \$DATA path.

- [`update_parameters()`](https://insightrx.github.io/pharmr.extra/reference/update_parameters.md)
  now also accepts a raw `nlmixr2FitCore` / `nlmixr2FitData` object —
  useful when fitting outside
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md).
  Both diagonal and off-diagonal omega elements are extracted and named
  per pharmpy’s `IIV_X` / `IIV_X_IIV_Y` convention, so block-omega
  covariances are now updated alongside variance terms (previously
  dropped).

- [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md),
  [`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md),
  and
  [`create_vpc_data()`](https://insightrx.github.io/pharmr.extra/reference/create_vpc_data.md)
  now dispatch on the model engine. Pharmpy nlmixr-format models are
  routed through nlmixr2 / rxode2 directly (no pharmpy `pyreadr`
  dependency). NONMEM models still use the existing nmfe / PsN / pharmpy
  paths unchanged.

- New `control` argument on
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  is forwarded to `nlmixr2::nlmixr2()` (e.g. `foceiControl()`); ignored
  for NONMEM.

- [`validate_model()`](https://insightrx.github.io/pharmr.extra/reference/validate_model.md)
  now accepts nlmixr-format pharmpy models.

- [`get_advan()`](https://insightrx.github.io/pharmr.extra/reference/get_advan.md)
  /
  [`get_obs_compartment()`](https://insightrx.github.io/pharmr.extra/reference/get_obs_compartment.md)
  return gracefully for non-NONMEM models instead of erroring on the
  missing control stream.

- [`add_sir()`](https://insightrx.github.io/pharmr.extra/reference/add_sir.md)
  now warns and no-ops for non-NONMEM models instead of silently doing
  nothing or aborting deep inside the covariance-record reader.

- [`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
  now forwards `esttool = "nlmixr"` to pharmpy when a search tool
  (`modelsearch`, `covsearch`, `iivsearch`, `ruvsearch`, `amd`,
  `bootstrap`) is invoked with an nlmixr-format model. Pharmpy can drive
  these searches against nlmixr2 if the Python package `pyreadr` is
  installed and the system Rscript that pharmpy spawns has a working
  nlmixr2 / data.table install.

- [`compare_nlme_runs()`](https://insightrx.github.io/pharmr.extra/reference/compare_nlme_runs.md)
  now detects the engine per run folder and loads nlmixr2 fits from the
  saved `<id>.rds` next to the run directory, so
  [`compare_nlme_runs()`](https://insightrx.github.io/pharmr.extra/reference/compare_nlme_runs.md)
  works for nlmixr2 runs too.

- [`create_modelfit_info_table()`](https://insightrx.github.io/pharmr.extra/reference/create_modelfit_info_table.md)
  is robust to fit objects reloaded from RDS whose pharmpy model
  attribute is no longer a live Python reference.

- [`create_vpc_data()`](https://insightrx.github.io/pharmr.extra/reference/create_vpc_data.md)
  on an nlmixr fit now uses the fitted model’s parameter estimates (was
  silently falling back to the pre-fit initial estimates when only a
  `fit` was supplied), and restricts `obs` to observation rows so it
  matches the simulation row set.

- [`run_nlme_nlmixr()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme_nlmixr.md)
  now stashes an explicit `data` argument on the model so saved fits
  sim/VPC against the right dataset (previously the saved model still
  referenced the original `model$dataset`).

- Initial CRAN submission.
