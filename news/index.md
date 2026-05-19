# Changelog

## pharmr.extra (development version)

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
