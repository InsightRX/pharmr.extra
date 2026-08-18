# R-level flag to avoid re-running the Python patch code every call.
.nlmixr_patch_applied <- local({
  applied <- FALSE
  list(
    get = function() applied,
    set = function() applied <<- TRUE
  )
})

#' Patch Pharmpy's nlmixr execution backend
#'
#' Applies Python monkey-patches to `pharmpy.tools.external.nlmixr.run`, the
#' module every nlmixr candidate fit dispatched by a Pharmpy tool
#' (`bootstrap`, `modelsearch`, `covsearch`, `iivsearch`, `amd`) goes through.
#' Three upstream bugs make it fail for essentially every real dataset, so
#' those tools are unusable against nlmixr-format models without this patch
#' (InsightRX/pharmr.extra#121):
#'
#' 1. `rdata["thetas"].loc[get_thetas(model).names]` — `Parameters$names` is a
#'    *tuple* in Pharmpy >= 2.0, and `pandas` reads a tuple passed to `.loc` as
#'    a multi-axis indexer, so this raises
#'    `IndexingError: Too many indexers`. (In Pharmpy 1.8 `names` is a list and
#'    the line happens to work.)
#' 2. `predictions.set_index(model.dataset[model.dataset["DV"] != 0].index)` —
#'    nlmixr2 returns one prediction row per *observation record*, but this
#'    indexes the dataset by "DV is non-zero". Any dataset with an observation
#'    whose `DV` is exactly 0 (BLQ imputed to 0, a baseline 0 sample, ...)
#'    therefore has fewer index labels than prediction rows and pandas raises
#'    `ValueError: Length mismatch`.
#' 3. `execute_model()` writes the candidate's dataset with
#'    `write_dataset()` / `write_csv()`, which names the file after the
#'    *datainfo* path, but the R script it generates reads
#'    `<model name>.csv`. Whenever the model carries a datainfo path — which
#'    every `modelsearch` candidate does, since they are derived from the input
#'    model — the two names differ and the R run dies with
#'    `cannot open file '.../<model name>.csv'`, leaving no results for the
#'    tool to rank.
#'
#' The patch fixes (1) by wrapping the `get_thetas` that `run.py` imported so
#' `names` is always a list, (2) by handing the parser a model whose `DV`
#' column is 1 on observation records and 0 elsewhere — so the upstream
#' `DV != 0` mask selects exactly the observation rows, with the dataset's own
#' index labels preserved (observation records are identified from `EVID` /
#' `MDV`; masks are tried in turn and the first one the parser accepts wins,
#' and if none work the unpatched call is made so the original Pharmpy error
#' surfaces) — and (3) by wrapping the dataset writer so that, when it is
#' handed a directory, it writes `<model name>.csv`: the name the generated R
#' script actually reads.
#'
#' Idempotent, and a no-op on a Pharmpy release that has fixed these bugs.
#' Called automatically by [call_pharmpy_tool()] for nlmixr-format models.
#'
#' @return `NULL`, invisibly. Called for its side effect on the Python session.
#'
#' @export
patch_pharmpy_nlmixr_results <- function() {
  if (.nlmixr_patch_applied$get()) return(invisible(NULL))

  reticulate::py_run_string("
import pharmpy.tools.external.nlmixr.run as _nlmixr_run

if not getattr(_nlmixr_run, '_pharmr_extra_nlmixr_patched', False):

    # ── Bug 1: `get_thetas(model).names` is a tuple in pharmpy >= 2.0 ────────
    # `rdata['thetas'].loc[<tuple>]` makes pandas read the tuple as a
    # multi-axis indexer -> `IndexingError: Too many indexers`. run.py uses
    # `get_thetas` in exactly one place, so wrapping the name it imported is
    # enough; `.names` is coerced to a list, everything else is delegated.
    _pharmr_extra_orig_get_thetas = _nlmixr_run.get_thetas

    class _PharmrExtraThetas:
        def __init__(self, params):
            object.__setattr__(self, '_params', params)

        def __getattr__(self, name):
            value = getattr(object.__getattribute__(self, '_params'), name)
            return list(value) if name == 'names' else value

        def __iter__(self):
            return iter(object.__getattribute__(self, '_params'))

        def __len__(self):
            return len(object.__getattribute__(self, '_params'))

        def __getitem__(self, key):
            return object.__getattribute__(self, '_params')[key]

    def _pharmr_extra_get_thetas(model):
        return _PharmrExtraThetas(_pharmr_extra_orig_get_thetas(model))

    _nlmixr_run.get_thetas = _pharmr_extra_get_thetas

    # ── Bug 2: observation records are selected with `DV != 0` ──────────────
    # nlmixr2 returns one prediction row per observation record, so the index
    # must be the observation rows of the dataset, not the non-zero-DV rows.
    # Rather than reimplementing the parser, hand it a model whose DV column
    # is 1 on observation records and 0 elsewhere: upstream's `DV != 0` mask
    # then selects exactly those rows, keeping the dataset's index labels.
    _pharmr_extra_orig_parse = _nlmixr_run.parse_modelfit_results

    def _pharmr_extra_obs_masks(df):
        masks = []
        has_evid = 'EVID' in df.columns
        has_mdv = 'MDV' in df.columns
        if has_evid and has_mdv:
            masks.append((df['EVID'] == 0) & (df['MDV'] == 0))
        if has_evid:
            masks.append(df['EVID'] == 0)
        if has_mdv:
            masks.append(df['MDV'] == 0)
        return masks

    def _pharmr_extra_parse_modelfit_results(model, path, *args, **kwargs):
        try:
            df = model.dataset
        except Exception:
            df = None
        if df is not None and 'DV' in df.columns:
            seen = set()
            for mask in _pharmr_extra_obs_masks(df):
                key = mask.to_numpy().tobytes()
                if key in seen:
                    continue
                seen.add(key)
                fixed = df.copy()
                fixed['DV'] = mask.astype(float)
                try:
                    model_obs = model.replace(dataset=fixed)
                    return _pharmr_extra_orig_parse(model_obs, path, *args, **kwargs)
                except Exception:
                    continue
        # No usable observation flag, or every mask was rejected: fall through
        # so the caller sees Pharmpy's own error rather than ours.
        return _pharmr_extra_orig_parse(model, path, *args, **kwargs)

    _nlmixr_run.parse_modelfit_results = _pharmr_extra_parse_modelfit_results

    # ── Bug 3: dataset written under a name the generated R script ─────────
    # does not read. `execute_model()` writes `<datainfo name>.csv` into the
    # run directory but generates `read.csv('<path>/<model name>.csv')`.
    # Wrap the writer so a directory destination always yields the latter.
    def _pharmr_extra_dataset_writer(orig):
        from pathlib import Path as _Path

        def _writer(model, path=None, force=False, **kwargs):
            if path is not None:
                dest = _Path(path)
                if dest.is_dir():
                    return orig(
                        model,
                        path=dest / '{}.csv'.format(model.name),
                        force=True,
                        **kwargs
                    )
            return orig(model, path=path, force=force, **kwargs)

        return _writer

    for _name in ('write_dataset', 'write_csv'):
        _orig_writer = getattr(_nlmixr_run, _name, None)
        if _orig_writer is not None:
            setattr(_nlmixr_run, _name, _pharmr_extra_dataset_writer(_orig_writer))

    _nlmixr_run._pharmr_extra_nlmixr_patched = True
")

  .nlmixr_patch_applied$set()
  invisible(NULL)
}
