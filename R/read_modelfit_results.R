#' Read NONMEM modelfit results, with bug fixes for SIR covariance models
#'
#' Wraps `pharmr::read_modelfit_results()` with patches for Pharmpy bugs that
#' arise when a SIR covariance step is used (`$COVARIANCE SIRSAMPLE=...`).
#' NONMEM writes a second ext-file table (method "Importance Sampling of
#' Variance-Covariance (SIR)") that Pharmpy mishandles in three ways:
#'
#' **Bug 1** (`_parse_parameter_estimates` — NaN final estimates): The SIR
#' table contains ITERATION 0 (FOCE starting point) and iterations 1…N (SIR
#' samples). Because the SIR modal OFV at `-1000000000` differs from the last
#' sample OFV, `_get_iter_df` appends a NaN sentinel row. This NaN row is the
#' last row of the combined `pe` DataFrame, so `pe.iloc[-1].isnull().all()` is
#' True and `final_pe` is set to all-NaN instead of the FOCE estimates.
#'
#' **Bug 2** (`_parse_table_numbers` — wrong covstatus): Including the SIR
#' table in `table_numbers` makes `covstatus_table_number` point to the SIR
#' table (number 2). The SIR block in the lst-file has "Elapsed estimation
#' time" but no "Elapsed covariance time", so `covariance_step_ok` is `None`
#' and the `.cov`/`.cor` files are never read, even though the standard
#' covariance step ran successfully under table 1.
#'
#' **Bug 3** (`_parse_standard_errors` — cov_abort with missing -1000000005):
#' If the SIR ext table has row `-1000000001` (SEs) but not `-1000000005`
#' (omega/sigma SEs in stdcorr form), the original code overrides the valid
#' SEs with NaN and sets `cov_abort = True`, blocking the `.cov` file read.
#'
#' The fixes skip the SIR table in `_parse_table_numbers`, `_parse_ofv`, and
#' `_parse_parameter_estimates` (so only the FOCE table is used for estimates
#' and covariance-step lookup), while `_parse_standard_errors` still uses the
#' last (SIR) table to extract SIR-based standard errors.
#'
#' @param path Path to NONMEM model file (e.g. `"run1/run.mod"`).
#' @param ... Additional arguments passed to `pharmr::read_modelfit_results()`.
#'
#' @returns A Pharmpy `ModelfitResults` object (same as
#'   `pharmr::read_modelfit_results()`).
#'
#' @export
read_modelfit_results <- function(path, ...) {
  .patch_pharmpy_sir_bugs()
  ## When `$ESTIMATION FILE=...` (e.g. PsN's `psn.ext`) redirects iteration
  ## output, pharmpy still derives the path it reads from the model stem and
  ## returns nothing on the empty default. Normalize here so every caller
  ## (run_nlme, direct user calls, downstream tools) benefits — not just the
  ## ones that go through the non-eval branch of run_nlme.
  ensure_default_ext_file(dirname(path), basename(path), verbose = FALSE)
  pharmr::read_modelfit_results(path, ...)
}

# R-level flag to avoid patching more than once per session.
.sir_patch_applied <- local({
  applied <- FALSE
  list(
    get = function() applied,
    set = function() applied <<- TRUE
  )
})

# Apply Python monkey-patches for Pharmpy SIR bugs (idempotent).
.patch_pharmpy_sir_bugs <- function() {
  if (.sir_patch_applied$get()) return(invisible(NULL))

  reticulate::py_run_string("
import pharmpy.tools.external.nonmem.results as _nm
from pharmpy.deps import numpy as _np
from pharmpy.deps import pandas as _pd
from pharmpy.model.external.nonmem.table import ExtTable as _ExtTable

# ── Helper: detect SIR tables ────────────────────────────────────────────────
def _is_sir_table(table):
    m = getattr(table, 'method', None)
    return m is not None and 'SIR' in m.upper()

# ── Bug 1 + 2: skip SIR tables in _parse_table_numbers ───────────────────────
# Removing the SIR table from table_numbers ensures covstatus_table_number
# points to the FOCE table (table 1), where 'Elapsed covariance time' appears
# in the lst-file TERE block, so the .cov/.cor files are read correctly.

def _patched_parse_table_numbers(ext_tables, subproblem):
    table_numbers = []
    for table in ext_tables.tables:
        if subproblem and table.subproblem != subproblem:
            continue
        if _is_sir_table(table):
            continue   # skip SIR table
        table_numbers.append(table.number)
    return table_numbers

_nm._parse_table_numbers = _patched_parse_table_numbers

# ── Bug 1 + 2: skip SIR tables in _parse_ofv ─────────────────────────────────
# Prevents the SIR table from becoming final_table (which would give a wrong
# or NaN final OFV) and keeps ofv_iterations clean.

def _patched_parse_ofv(ext_tables, subproblem):
    step = []
    iteration = []
    ofv = []
    final_table = None
    for table_number, table in enumerate(ext_tables.tables, start=1):
        if (subproblem and table.subproblem != subproblem) or table.design_optimality is not None:
            continue
        if _is_sir_table(table):
            continue   # skip SIR table
        df = _nm._get_iter_df(table.data_frame)
        n = len(df)
        step += [table_number] * n
        iteration += list(df['ITERATION'])
        ofv += list(df['OBJ'])
        final_table = table

    assert isinstance(final_table, _ExtTable)
    if _np.isnan(ofv[-1]):
        final_ofv = ofv[-1]
    else:
        final_ofv = final_table.final_ofv
    ofv_iterations = _nm.create_ofv_iterations_series(ofv, step, iteration)
    return float(final_ofv), ofv_iterations

_nm._parse_ofv = _patched_parse_ofv

# ── Bug 1 + 2: skip SIR tables in _parse_parameter_estimates ─────────────────
# Without this, the SIR table's 500 sample iterations are processed and a NaN
# sentinel row ends up as pe.iloc[-1], making final_pe all-NaN.

def _patched_parse_parameter_estimates(control_stream, name_map, ext_tables, subproblem, parameters):
    pe = _pd.DataFrame()
    fixed_param_names = []
    final_table = None
    fix = None
    for table_number, table in enumerate(ext_tables.tables, start=1):
        if (subproblem and table.subproblem != subproblem) or table.design_optimality is not None:
            continue
        if _is_sir_table(table):
            continue   # skip SIR table
        df = _nm._get_iter_df(table.data_frame)
        assert isinstance(table, _ExtTable)
        fix = _nm._get_fixed_parameters(table, parameters, name_map)
        fixed_param_names = [name for name in list(df.columns)[1:-1] if fix[name]]
        df = df.drop(fixed_param_names + ['OBJ'], axis=1)
        df['step'] = table_number
        df = df.rename(columns=name_map)
        pe = _pd.concat([pe, df])
        final_table = table

    assert fix is not None
    assert final_table is not None
    if pe.iloc[-1].drop(['ITERATION', 'step']).isnull().all():
        final_pe = pe.iloc[-1].drop(['ITERATION', 'step']).rename('estimates')
    else:
        final_pe = final_table.final_parameter_estimates
        final_pe = final_pe.drop(fixed_param_names)
        final_pe = final_pe.rename(index=name_map)
    pe = pe.rename(columns={'ITERATION': 'iteration'}).set_index(['step', 'iteration'])

    try:
        sdcorr = final_table.omega_sigma_stdcorr[~fix]
    except KeyError:
        sdcorr_ests = _pd.Series(_np.nan, index=pe.index)
    else:
        sdcorr = sdcorr.rename(index=name_map)
        sdcorr_ests = final_pe.copy()
        sdcorr_ests.update(sdcorr)
    return final_pe, sdcorr_ests, pe

_nm._parse_parameter_estimates = _patched_parse_parameter_estimates

# ── Bug 3: _parse_standard_errors — keep valid SEs when -1000000005 absent ───
# When the SIR ext table has -1000000001 (SEs) but not -1000000005 (stdcorr
# omega/sigma SEs), the original except branch overrides the valid ses with NaN
# and sets cov_abort=True, which blocks reading the .cov file.
# Fix: keep the valid ses and leave cov_abort=False.

def _get_fixed_parameters_local(table, parameters, pe_translation):
    try:
        return table.fixed
    except KeyError:
        ests = table.final_parameter_estimates
        fixed = _pd.Series(parameters.fix)
        fixed = fixed.rename({v: k for k, v in pe_translation.items()})
        return _pd.concat([fixed, _pd.Series(True, index=ests.index.difference(fixed.index))])

def _patched_parse_standard_errors(control_stream, name_map, ext_tables, parameters, pe):
    cov_abort = False
    table = ext_tables.tables[-1]
    assert isinstance(table, _ExtTable)
    try:
        ses = table.standard_errors
    except KeyError:
        ses = _pd.Series(_np.nan, index=pe.index, name='SE')
        sdcorr_ses = _pd.Series(_np.nan, index=pe.index, name='SE_sdcorr')
        return ses, sdcorr_ses, cov_abort   # cov_abort stays False

    fix = _get_fixed_parameters_local(table, parameters, name_map)
    ses = ses[~fix]
    try:
        sdcorr = table.omega_sigma_se_stdcorr[~fix]
    except KeyError:
        # -1000000005 not present (SIR table may omit it): keep valid ses.
        ses = ses.rename(index=name_map)
        sdcorr_ses = _pd.Series(_np.nan, index=pe.index, name='SE_sdcorr')
        return ses, sdcorr_ses, cov_abort   # cov_abort stays False (FIX)
    else:
        ses = ses.rename(index=name_map)
        sdcorr = sdcorr.rename(index=name_map)
        sdcorr_ses = ses.copy()
        sdcorr_ses.update(sdcorr)
        sdcorr_ses = sdcorr_ses.rename(index=name_map)
    return ses, sdcorr_ses, cov_abort

_nm._parse_standard_errors = _patched_parse_standard_errors
")

  .sir_patch_applied$set()
  invisible(NULL)
}
