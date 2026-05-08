# pharmr.extra (development version)

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
* `call_pharmpy_tool()` errors clearly when a NONMEM-only pharmpy tool
  (`modelsearch`, `covsearch`, `iivsearch`, `ruvsearch`, `amd`, `bootstrap`)
  is invoked with an nlmixr-format model.
* `compare_nlme_runs()` now detects the engine per run folder and loads
  nlmixr2 fits from the saved `<id>.rds` next to the run directory, so
  `compare_nlme_runs()` works for nlmixr2 runs too.
* `create_modelfit_info_table()` is robust to fit objects reloaded from RDS
  whose pharmpy model attribute is no longer a live Python reference.

* Initial CRAN submission.
