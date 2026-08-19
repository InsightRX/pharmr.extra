# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

``` r

# Install dependencies and load package
devtools::install_deps()
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
devtools::test(filter = "create_model")

# Run R CMD CHECK
devtools::check()

# Update documentation (NAMESPACE + man/)
devtools::document()
```

## Development rules

- **Bump the package version** (in `DESCRIPTION`) for every code change.
- **After changing any function documentation**, regenerate docs with
  `devtools::document()`.

## Architecture

`pharmr.extra` is an R package that extends
[pharmr](https://github.com/InsightRX/pharmr) — R bindings to Python’s
Pharmpy PK modeling library — with higher-level utilities and bug
workarounds.

**Core layer**: Functions call Python via `reticulate` (virtualenv at
`~/.virtualenvs/r-reticulate`). The pharmr package handles the base
Python bindings; this package adds orchestration on top.

**Two model backends**: NONMEM (via Pharmpy) and nlmixr2 (pure R). Key
entry points: -
[`create_model()`](https://insightrx.github.io/pharmr.extra/reference/create_model.md)
— Builds a Pharmpy model object (main heavy function in
`R/create_model.R`) -
[`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
— Fits a model (preferred over deprecated
[`fit_model()`](https://insightrx.github.io/pharmr.extra/reference/fit_model.md)) -
[`run_sim()`](https://insightrx.github.io/pharmr.extra/reference/run_sim.md)
/
[`run_simulation()`](https://insightrx.github.io/pharmr.extra/reference/run_simulation.md)
— Simulation utilities

**Pharmpy API compatibility**: Several functions handle differences
between pharmpy v1.x and v2.0+ (e.g., `set_dv.R` uses `ColumnInfo` type
detection). When touching dataset/datainfo manipulation, check both API
paths.

**Known bug workarounds**: `R/read_modelfit_results.R` overrides
[`pharmr::read_modelfit_results()`](https://rdrr.io/pkg/pharmr/man/read_modelfit_results.html)
with fixes for SAEM+IMP multi-step estimation failures. See the memory
file for details on the four bugs patched there.
`R/patch_pharmpy_nlmixr.R` monkey-patches Pharmpy’s nlmixr backend
(`pharmpy.tools.external.nlmixr.run`) so `bootstrap`/`modelsearch` and
the other search tools work against nlmixr2 models;
[`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
applies it automatically for nlmixr-format models.

## Testing conventions

- Tests skip automatically when NONMEM/Pharmpy is unavailable:
  `skip_if_nonmem_not_available()`
- Mock model builders: `make_model_with_cov()`,
  `make_model_without_cov()` in `tests/testthat/helper.R`
- Snapshot tests are used for complex output comparison
- Test fixtures live in `tests/testthat/fixtures/`

## CI

GitHub Actions (`.github/workflows/`): - `R-CMD-check.yaml`: runs on
Ubuntu, installs pharmpy via
[`pharmr::install_pharmpy()`](https://rdrr.io/pkg/pharmr/man/install_pharmpy.html),
pins pandas to 2.3.3 - `test-coverage.yaml`: manual dispatch only,
uploads to Codecov
