#' Residual column names recognized in NONMEM / nlmixr2 output tables
#'
#' Superset of the columns Pharmpy looks at (`RES`, `WRES`, `CWRES`), since
#' `add_default_output_tables()` writes `CWRES`, `CIWRES` and `NPDE`, and
#' nlmixr2 fits report `IRES`/`IWRES`/`CWRES`.
#'
#' @noRd
residual_column_names <- function() {
  c(
    "RES", "IRES", "WRES", "IWRES", "CRES", "CWRES", "CIWRES",
    "CWRESI", "IWRESI", "EWRES", "ECWRES", "NPDE", "NPD"
  )
}

#' Build a joinable residuals frame from a fit's output tables
#'
#' Returns one row per observation record of `dataset`, with the row number in
#' `dataset` (`ROW`), the ID and independent-variable columns as join keys, and
#' every residual column found in `tables`.
#'
#' Tables are matched against the dataset in three ways, in order:
#'   1. one row per dataset record (the NONMEM `$TABLE` convention) — the
#'      observation rows are subset out;
#'   2. one row per observation record (the `as.data.frame(nlmixr2fit)`
#'      convention) — used as-is;
#'   3. otherwise, aligned on the ID/IDV key (NA where a record has no
#'      matching table row). Only the first match is used per key, so this
#'      degrades to NA rather than guessing when keys are duplicated, and the
#'      table is ignored altogether when fewer than half the observation
#'      records are found (i.e. it isn't from this run).
#'
#' Returns `NULL` when no residual column can be located, so callers can leave
#' the fit untouched.
#'
#' @param dataset Model dataset as a data.frame (i.e. `model$dataset`).
#' @param tables Named list of output tables, as attached by
#'   [get_tables_from_fit()].
#' @param id_col Name of the ID column in `dataset` and the tables.
#' @param idv_col Name of the independent variable (time) column.
#'
#' @noRd
build_keyed_residuals <- function(
  dataset,
  tables,
  id_col = "ID",
  idv_col = "TIME"
) {
  if(is.null(dataset) || !is.data.frame(dataset) || nrow(dataset) == 0) {
    return(NULL)
  }
  if(is.null(tables) || length(tables) == 0) {
    return(NULL)
  }
  obs_idx <- find_observation_rows(dataset)
  if(is.null(obs_idx)) obs_idx <- seq_len(nrow(dataset))
  if(length(obs_idx) == 0) return(NULL)

  has_key <- all(c(id_col, idv_col) %in% names(dataset))
  if(has_key) {
    key_obs <- paste(dataset[[id_col]][obs_idx], dataset[[idv_col]][obs_idx])
  }

  res_cols <- list()
  for(tab in tables) {
    if(!is.data.frame(tab) || nrow(tab) == 0) next
    cols <- setdiff(
      intersect(residual_column_names(), names(tab)),
      names(res_cols)
    )
    if(length(cols) == 0) next
    if(nrow(tab) == nrow(dataset)) {
      vals <- tab[obs_idx, cols, drop = FALSE]
    } else if(nrow(tab) == length(obs_idx)) {
      vals <- tab[, cols, drop = FALSE]
    } else if(has_key && all(c(id_col, idv_col) %in% names(tab))) {
      idx <- match(key_obs, paste(tab[[id_col]], tab[[idv_col]]))
      ## Guard against tables that aren't from this run at all: require at
      ## least half of the observation records to be found.
      if(sum(!is.na(idx)) < length(idx) / 2) next
      vals <- tab[idx, cols, drop = FALSE]
    } else {
      next
    }
    for(col in cols) res_cols[[col]] <- unname(vals[[col]])
  }
  if(length(res_cols) == 0) return(NULL)

  keys <- list(ROW = obs_idx)
  if(id_col %in% names(dataset)) keys[[id_col]] <- dataset[[id_col]][obs_idx]
  if(idv_col %in% names(dataset)) keys[[idv_col]] <- dataset[[idv_col]][obs_idx]
  out <- as.data.frame(
    c(keys, res_cols),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Replace the `residuals` slot on a fit with a joinable version
#'
#' Pharmpy's NONMEM parser exposes `residuals` as a DataFrame indexed by
#' dataset row label, with rows removed by a `(df != 0).any(axis=1)` heuristic
#' (`_parse_residuals()`). That breaks R callers twice over:
#'
#'   * reticulate drops the pandas index on conversion, so the only join key
#'     is gone and `fit$residuals` cannot be attached to the dataset;
#'   * the heuristic also drops *observation* records whose residual columns
#'     are all exactly 0 (NONMEM writes 0 for records it did not compute a
#'     residual for), so the row count doesn't match the observation records
#'     either — `nrow(fit$residuals)` was 1134 for a 2184-observation dataset
#'     (#120).
#'
#' This rebuilds the slot from the run's output tables: one row per
#' observation record, with `ROW`/ID/IDV keys (see [build_keyed_residuals()]).
#' For NONMEM fits the pandas index is set to the corresponding
#' `model.dataset` labels, so Pharmpy code that joins on `residuals.index`
#' (e.g. `plot_cwres_vs_idv()`) and code that assumes the non-observations
#' have been filtered (e.g. `ruvsearch`) keep working.
#'
#' Leaves the fit untouched when no residual column can be found in the
#' tables, or when the Pharmpy object cannot be rebuilt.
#'
#' @param fit Fit object: either a Pharmpy `ModelfitResults` object or an
#'   nlmixr2-shaped fit list.
#' @param model Pharmpy model object the fit belongs to.
#' @param tables Named list of output tables, from [get_tables_from_fit()].
#' @param verbose Verbose output?
#'
#' @noRd
repair_residuals <- function(fit, model, tables, verbose = FALSE) {
  if(is.null(fit) || is.null(model)) return(fit)
  dataset <- tryCatch(model$dataset, error = function(e) NULL)
  key_cols <- dataset_key_columns(model)
  res <- tryCatch(
    build_keyed_residuals(
      dataset,
      tables,
      id_col = key_cols$id,
      idv_col = key_cols$idv
    ),
    error = function(e) NULL
  )
  if(is.null(res) || nrow(res) == 0) {
    return(fit)
  }

  if(inherits(fit, "python.builtin.object")) {
    new_fit <- tryCatch(
      set_pharmpy_residuals(fit, model, res),
      error = function(e) {
        if(verbose) {
          cli::cli_alert_warning(
            "Could not update `residuals` on fit object: {conditionMessage(e)}"
          )
        }
        NULL
      }
    )
    if(!is.null(new_fit)) fit <- new_fit
  } else if(is.list(fit) &&
            (inherits(fit, "nlmixr2_modelfit_results") ||
             "residuals" %in% names(fit))) {
    ## Only fit-shaped lists: an evaluation-only run returns an empty list,
    ## which should stay empty.
    fit$residuals <- res
  }
  fit
}

#' Get the ID and independent-variable column names of a model
#'
#' Falls back to the NONMEM defaults `ID` / `TIME` when the datainfo cannot be
#' read (e.g. a fit object without a usable model attached).
#'
#' @noRd
dataset_key_columns <- function(model) {
  id <- tryCatch(model$datainfo$id_column$name, error = function(e) NULL)
  idv <- tryCatch(model$datainfo$idv_column$name, error = function(e) NULL)
  list(
    id = if(is.null(id) || !is.character(id)) "ID" else id,
    idv = if(is.null(idv) || !is.character(idv)) "TIME" else idv
  )
}

#' Return a copy of a Pharmpy ModelfitResults with new `residuals`
#'
#' `ModelfitResults` is a frozen dataclass, so the slot is replaced with
#' `dataclasses.replace()`. The pandas index of the new frame is set to the
#' `model.dataset` row labels of the observation records (taken from the `ROW`
#' column), which is what Pharmpy itself uses as join key.
#'
#' @noRd
set_pharmpy_residuals <- function(fit, model, residuals) {
  .define_residuals_helper()
  py_main <- reticulate::import_main()
  py_main$`_pharmr_extra_set_residuals`(
    fit,
    model,
    residuals,
    as.integer(residuals$ROW)
  )
}

# R-level flag to avoid re-defining the Python helper in every call.
.residuals_helper_defined <- local({
  defined <- FALSE
  list(
    get = function() defined,
    set = function() defined <<- TRUE
  )
})

# Define the Python helper that rebuilds the ModelfitResults (idempotent).
.define_residuals_helper <- function() {
  if(.residuals_helper_defined$get()) return(invisible(NULL))

  reticulate::py_run_string("
import dataclasses as _dataclasses
from pharmpy.deps import pandas as _pd

def _pharmr_extra_set_residuals(results, model, residuals, positions):
    df = residuals if isinstance(residuals, _pd.DataFrame) else _pd.DataFrame(residuals)
    df = df.copy()
    # Index by dataset row label, like pharmpy's own _parse_tables() does, so
    # that `dataset.loc[residuals.index]` still works on the returned object.
    try:
        dataset = model.dataset
        rows = [int(p) - 1 for p in positions]
        if dataset is not None and len(rows) > 0 and max(rows) < len(dataset):
            df.index = dataset.index[rows]
    except Exception:
        pass
    return _dataclasses.replace(results, residuals=df)
")

  .residuals_helper_defined$set()
  invisible(NULL)
}
