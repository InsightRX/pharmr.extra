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

#' Normalize a key column to a character vector that survives table round-trip
#'
#' NONMEM writes table columns at ~5 significant digits (`1PE11.4`), so a
#' dataset `TIME` of `0.0833333` comes back from the table as `8.33330E-02`.
#' Pasting the raw values would never match, so both sides are rounded to 5
#' significant digits first. Character columns that hold numbers (`"001"` vs
#' `1`) are coerced the same way, so an ID read as character still matches an
#' ID read as numeric.
#'
#' @noRd
normalize_key_column <- function(x) {
  if(is.factor(x)) x <- as.character(x)
  if(is.numeric(x)) {
    ## as.character() formats element-wise, unlike format(), so two vectors
    ## with different value ranges still produce comparable strings.
    return(as.character(signif(x, 5)))
  }
  chr <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(chr))
  if(!any(is.na(num) & !is.na(chr) & chr != "")) {
    return(as.character(signif(num, 5)))
  }
  chr
}

#' Build the ID/IDV record key used to align a table with the dataset
#'
#' @noRd
record_key <- function(id, idv) {
  paste(normalize_key_column(id), normalize_key_column(idv), sep = "\r")
}

#' Make a record key unique by appending the occurrence number
#'
#' Datasets legitimately repeat ID/IDV pairs (e.g. two analytes measured at the
#' same time). Numbering the occurrences keeps [base::match()] from collapsing
#' them all onto the first table row: the n-th record with a given key is
#' matched to the n-th table row with that key.
#'
#' @noRd
disambiguate_key <- function(key) {
  if(!anyDuplicated(key)) return(key)
  paste(key, stats::ave(seq_along(key), key, FUN = seq_along), sep = "\r")
}

#' Take the last block of a table that holds several stacked repetitions
#'
#' [read_table_nm()] skips only the *first* `TABLE NO.` header and `na.omit()`s
#' the rest, so a `$TABLE` file written by a multi-step estimation (e.g. SAEM
#' followed by IMP) comes back as several copies of the table stacked on top of
#' each other. The last block is the one from the final estimation step.
#'
#' @param tab Table as read from the run folder.
#' @param lengths Candidate block lengths, most specific first.
#'
#' @noRd
last_table_block <- function(tab, lengths) {
  n <- nrow(tab)
  ## An exact match is a single block, never a stack — check before looking
  ## for multiples (a 6-record dataset with 3 observations would otherwise
  ## have its 6-row table cut down to the last 3 rows).
  if(n %in% lengths) return(tab)
  for(len in lengths) {
    if(len > 0 && n > len && n %% len == 0) {
      return(tab[seq.int(n - len + 1, n), , drop = FALSE])
    }
  }
  tab
}

#' Does a table carry both key columns?
#'
#' @noRd
has_key_columns <- function(tab, id_col, idv_col) {
  all(c(id_col, idv_col) %in% names(tab))
}

#' Build a joinable residuals frame from a fit's output tables
#'
#' Returns one row per observation record of `dataset`, with the row number in
#' `dataset` (`ROW`), the ID and independent-variable columns as join keys, and
#' every residual column found in `tables`.
#'
#' Tables written by a multi-step estimation hold several stacked copies of the
#' table (see [last_table_block()]); only the final block is used.
#'
#' Tables are then matched against the dataset in three ways, in order:
#'   1. one row per dataset record (the NONMEM `$TABLE` convention) — the
#'      observation rows are subset out;
#'   2. one row per observation record (the `as.data.frame(nlmixr2fit)`
#'      convention) — used as-is;
#'   3. otherwise, aligned on the ID/IDV key (NA where a record has no
#'      matching table row).
#'
#' Options 1 and 2 bind positionally, so when the table carries the key columns
#' the key is verified first and alignment falls through to option 3 on
#' mismatch (`as.data.frame(nlmixr2fit)` returns rows in ID-sorted order, which
#' need not be the dataset order). Keys are compared at 5 significant digits,
#' the precision NONMEM writes tables at, and repeated ID/IDV pairs are matched
#' in order of occurrence. A table in which fewer than half the observation
#' records are found is ignored altogether (i.e. it isn't from this run).
#'
#' The row-number column is named `ROW` unless `dataset` already has a column
#' of that name, in which case `.ROW` is used and a warning is issued — joining
#' the dataset's own `ROW` values against row numbers would silently mismatch.
#' The name used is returned as the `row_col` attribute of the result.
#'
#' Returns `NULL` when no residual column can be located, so callers can leave
#' the fit untouched.
#'
#' @param dataset Model dataset as a data.frame (i.e. `model$dataset`).
#' @param tables Named list of output tables, as attached by
#'   [get_tables_from_fit()].
#' @param id_col Name of the ID column in `dataset` and the tables.
#' @param idv_col Name of the independent variable (time) column.
#' @param verbose Verbose output?
#'
#' @noRd
build_keyed_residuals <- function(
  dataset,
  tables,
  id_col = "ID",
  idv_col = "TIME",
  verbose = FALSE
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

  has_key <- has_key_columns(dataset, id_col, idv_col)
  if(has_key) {
    key_obs <- record_key(
      dataset[[id_col]][obs_idx],
      dataset[[idv_col]][obs_idx]
    )
    key_obs_uniq <- disambiguate_key(key_obs)
  }

  res_cols <- list()
  for(i in seq_along(tables)) {
    tab <- tables[[i]]
    if(!is.data.frame(tab) || nrow(tab) == 0) next
    cols <- setdiff(
      intersect(residual_column_names(), names(tab)),
      names(res_cols)
    )
    if(length(cols) == 0) next

    ## Multi-step estimations stack one copy of the table per step; keep the
    ## last (i.e. the final estimation step).
    n_before <- nrow(tab)
    tab <- last_table_block(tab, c(nrow(dataset), length(obs_idx)))
    if(verbose && nrow(tab) < n_before) {
      tab_label <- names(tables)[i]
      if(is.null(tab_label) || is.na(tab_label) || tab_label == "") {
        tab_label <- as.character(i)
      }
      cli::cli_alert_info(
        "Table {.val {tab_label}} holds {n_before / nrow(tab)} stacked copies (multi-step estimation); using the last."
      )
    }

    tab_has_key <- has_key && has_key_columns(tab, id_col, idv_col)
    vals <- NULL
    if(nrow(tab) == nrow(dataset)) {
      ok <- !tab_has_key ||
        identical(
          record_key(
            tab[[id_col]][obs_idx],
            tab[[idv_col]][obs_idx]
          ),
          key_obs
        )
      if(ok) vals <- tab[obs_idx, cols, drop = FALSE]
    }
    if(is.null(vals) && nrow(tab) == length(obs_idx)) {
      ok <- !tab_has_key ||
        identical(record_key(tab[[id_col]], tab[[idv_col]]), key_obs)
      if(ok) vals <- tab[, cols, drop = FALSE]
    }
    if(is.null(vals) && tab_has_key) {
      idx <- match(
        key_obs_uniq,
        disambiguate_key(record_key(tab[[id_col]], tab[[idv_col]]))
      )
      ## Guard against tables that aren't from this run at all: require at
      ## least half of the observation records to be found.
      if(sum(!is.na(idx)) < length(idx) / 2) next
      vals <- tab[idx, cols, drop = FALSE]
    }
    if(is.null(vals)) next
    for(col in cols) res_cols[[col]] <- unname(vals[[col]])
  }
  if(length(res_cols) == 0) return(NULL)

  ## Don't shadow a `ROW` column the dataset already carries: its values are
  ## not row numbers once the dataset has been filtered (IGNORE=, ...), so a
  ## join on it would silently mismatch.
  row_col <- "ROW"
  if(row_col %in% names(dataset)) {
    row_col <- ".ROW"
    cli::cli_warn(c(
      "The dataset already has a {.field ROW} column, which does not necessarily hold row numbers.",
      i = "The residuals row-number key is named {.field .ROW} instead."
    ))
  }

  keys <- stats::setNames(list(obs_idx), row_col)
  if(id_col %in% names(dataset)) keys[[id_col]] <- dataset[[id_col]][obs_idx]
  if(idv_col %in% names(dataset)) keys[[idv_col]] <- dataset[[idv_col]][obs_idx]
  out <- as.data.frame(
    c(keys, res_cols),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(out) <- NULL
  attr(out, "row_col") <- row_col
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
#' Warns and leaves the fit untouched when no residual column can be found in
#' the tables, or when the Pharmpy object cannot be rebuilt.
#'
#' @param fit Fit object: either a Pharmpy `ModelfitResults` object or an
#'   nlmixr2-shaped fit list.
#' @param model Pharmpy model object the fit belongs to.
#' @param tables Named list of output tables, from [get_tables_from_fit()].
#' @param dataset Dataset the model was actually fitted to. Defaults to
#'   `model$dataset`; the nlmixr2 path passes the resolved fit data, which can
#'   differ (an explicit `data =` argument, or `attr(model, "original_data")`).
#' @param verbose Verbose output?
#'
#' @noRd
repair_residuals <- function(
  fit,
  model,
  tables,
  dataset = NULL,
  verbose = FALSE
) {
  if(is.null(fit) || is.null(model)) return(fit)
  if(is.null(dataset)) {
    dataset <- tryCatch(model$dataset, error = function(e) NULL)
  }
  if(!is.null(dataset) && !is.data.frame(dataset)) {
    dataset <- tryCatch(as.data.frame(dataset), error = function(e) NULL)
  }
  key_cols <- dataset_key_columns(model)
  res <- tryCatch(
    build_keyed_residuals(
      dataset,
      tables,
      id_col = key_cols$id,
      idv_col = key_cols$idv,
      verbose = verbose
    ),
    error = function(e) NULL
  )
  if(is.null(res) || nrow(res) == 0) {
    if(fit_has_residuals(fit)) {
      cli::cli_warn(c(
        "Could not rebuild a joinable {.field residuals} frame from the output tables.",
        i = "{.code fit$residuals} is left as returned by the fitting engine: it carries no join key and may not align with the observation records."
      ))
    }
    return(fit)
  }

  if(inherits(fit, "python.builtin.object")) {
    new_fit <- tryCatch(
      set_pharmpy_residuals(fit, model, res),
      error = function(e) {
        cli::cli_warn(c(
          "Could not update {.field residuals} on the fit object: {conditionMessage(e)}",
          i = "{.code fit$residuals} is left as returned by Pharmpy."
        ))
        NULL
      }
    )
    if(!is.null(new_fit)) {
      ## `dataclasses.replace()` returns a new object, so carry over any
      ## attributes a caller had already attached to the old one.
      fit <- copy_fit_attributes(from = fit, to = new_fit)
    }
  } else if(is.list(fit) &&
            (inherits(fit, "nlmixr2_modelfit_results") ||
             "residuals" %in% names(fit))) {
    ## Only fit-shaped lists: an evaluation-only run returns an empty list,
    ## which should stay empty.
    attr(res, "row_col") <- NULL
    fit$residuals <- res
  }
  fit
}

#' Does a fit carry a non-empty `residuals` slot?
#'
#' Used to decide whether a failed repair is worth warning about: there is
#' nothing to warn about when the fit never had residuals (e.g. an
#' evaluation-only run).
#'
#' @noRd
fit_has_residuals <- function(fit) {
  res <- tryCatch(fit$residuals, error = function(e) NULL)
  if(is.null(res)) return(FALSE)
  if(is.data.frame(res)) return(nrow(res) > 0)
  TRUE
}

#' Copy user attributes from one fit object onto another
#'
#' @noRd
copy_fit_attributes <- function(from, to) {
  attrs <- attributes(from)
  attrs <- attrs[setdiff(
    names(attrs),
    c("class", "names", "row.names", "dim", "dimnames")
  )]
  for(nm in names(attrs)) attr(to, nm) <- attrs[[nm]]
  to
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
#' `model.dataset` row labels of the observation records (taken from the row
#' number column), which is what Pharmpy itself uses as join key. When that
#' index cannot be established the helper raises rather than falling back to a
#' default `RangeIndex`: Pharmpy consumers do `model.dataset.loc[res.index]`,
#' which would then silently select the first `n` dataset rows.
#'
#' @noRd
set_pharmpy_residuals <- function(fit, model, residuals) {
  .define_residuals_helper()
  row_col <- attr(residuals, "row_col") %||% "ROW"
  rows <- residuals[[row_col]]
  attr(residuals, "row_col") <- NULL
  py_main <- reticulate::import_main()
  py_main$`_pharmr_extra_set_residuals`(
    fit,
    model,
    residuals,
    as.integer(rows)
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
    # Raise rather than leaving a default RangeIndex in place: pharmpy joins
    # with `model.dataset.loc[residuals.index]`, which would silently select
    # the first len(df) dataset rows (dose records included).
    dataset = model.dataset
    if dataset is None:
        raise ValueError('model has no dataset; cannot index residuals')
    rows = [int(p) - 1 for p in positions]
    if len(rows) == 0:
        raise ValueError('no observation rows to index residuals by')
    if min(rows) < 0 or max(rows) >= len(dataset):
        raise ValueError('residual row numbers fall outside the model dataset')
    df.index = dataset.index[rows]
    return _dataclasses.replace(results, residuals=df)
")

  .residuals_helper_defined$set()
  invisible(NULL)
}
