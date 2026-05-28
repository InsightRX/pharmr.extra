# pharmr.extra (development version)

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
