# Patch Pharmpy's nlmixr execution backend

Applies Python monkey-patches to `pharmpy.tools.external.nlmixr.run`,
the module every nlmixr candidate fit dispatched by a Pharmpy tool
(`bootstrap`, `modelsearch`, `covsearch`, `iivsearch`, `amd`) goes
through. Three upstream bugs make it fail for essentially every real
dataset, so those tools are unusable against nlmixr-format models
without this patch (InsightRX/pharmr.extra#121):

## Usage

``` r
patch_pharmpy_nlmixr_results()
```

## Value

`NULL`, invisibly. Called for its side effect on the Python session.

## Details

1.  `rdata["thetas"].loc[get_thetas(model).names]` — `Parameters$names`
    is a *tuple* in Pharmpy \>= 2.0, and `pandas` reads a tuple passed
    to `.loc` as a multi-axis indexer, so this raises
    `IndexingError: Too many indexers`. (In Pharmpy 1.8 `names` is a
    list and the line happens to work.)

2.  `predictions.set_index(model.dataset[model.dataset["DV"] != 0].index)`
    — nlmixr2 returns one prediction row per *observation record*, but
    this indexes the dataset by "DV is non-zero". Any dataset with an
    observation whose `DV` is exactly 0 (BLQ imputed to 0, a baseline 0
    sample, ...) therefore has fewer index labels than prediction rows
    and pandas raises `ValueError: Length mismatch`.

3.  `execute_model()` writes the candidate's dataset with
    `write_dataset()` / `write_csv()`, which names the file after the
    *datainfo* path, but the R script it generates reads
    `<model name>.csv`. Whenever the model carries a datainfo path —
    which every `modelsearch` candidate does, since they are derived
    from the input model — the two names differ and the R run dies with
    `cannot open file '.../<model name>.csv'`, leaving no results for
    the tool to rank.

The patch fixes (1) by wrapping the `get_thetas` that `run.py` imported
so `names` is always a list, (2) by handing the parser a model whose
`DV` column is 1 on observation records and 0 elsewhere — so the
upstream `DV != 0` mask selects exactly the observation rows, with the
dataset's own index labels preserved (observation records are identified
from `EVID` / `MDV`; masks are tried in turn and the first one the
parser accepts wins, and if none work the unpatched call is made so the
original Pharmpy error surfaces) — and (3) by wrapping the dataset
writer so that, when it is handed a directory, it writes
`<model name>.csv`: the name the generated R script actually reads,
keeping the caller's `force` so a leftover file in a reused run folder
still raises.

Because upstream only accepts a mask whose row count matches the number
of prediction rows, a mask that parses matched on count alone. If a
*different* mask selects the same number of (different) rows, the choice
is ambiguous and a `RuntimeWarning` is emitted rather than returning
silently misaligned predictions.

The patched `parse_modelfit_results` is installed both on
`pharmpy.tools.external.nlmixr.run` and on the
`pharmpy.tools.external.nlmixr` package alias — the latter is the
binding `pharmpy.tools.read_modelfit_results()` resolves, so without it
reading an nlmixr fit outside `execute_model()` stays broken. It also
tolerates the `strict=` keyword that alias passes but `run.py` does not
accept.

Idempotent, and a no-op on a Pharmpy release that has fixed these bugs.
Called automatically by
[`call_pharmpy_tool()`](https://insightrx.github.io/pharmr.extra/reference/call_pharmpy_tool.md)
for nlmixr-format models.
