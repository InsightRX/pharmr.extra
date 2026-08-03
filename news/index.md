# Changelog

## pharmr.extra (development version)

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
