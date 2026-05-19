# pharmr.extra (development version)

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
