## Workarounds for pharmpy's MFL parser, which unconditionally uppercases every
## identifier in a `search_space` string (see
## https://github.com/pharmpy/pharmpy/issues/4576). To make tools like covsearch
## work with datasets that use lowercase column names, we uppercase both the
## search_space and the model's $INPUT / datainfo / dataset columns to match.

## Return the unique lowercase-containing identifiers found in a search_space
## string. Used to warn the caller that the MFL parser will uppercase those
## tokens regardless of what they wrote.
detect_lowercase_identifiers <- function(search_space) {
  if (is.null(search_space) || !is.character(search_space)) {
    return(character(0))
  }
  tokens <- unlist(stringr::str_extract_all(
    search_space, "[A-Za-z_][A-Za-z0-9_]*"
  ))
  unique(tokens[stringr::str_detect(tokens, "[a-z]")])
}

## Uppercase every column name in a pharmpy model: both datainfo (so $INPUT is
## written uppercase) and the in-memory dataset. Returns a list with the new
## model and a named character vector of `old = new` renames that were applied.
##
## Performed entirely on the Python side via py_run_string to avoid R↔Python
## auto-conversion stripping the ColumnInfo type from list elements.
uppercase_model_columns <- function(model) {
  reticulate::py_run_string("
def _pharmr_extra_uppercase_model(m):
    rm = {c.name: c.name.upper()
          for c in m.datainfo
          if c.name != c.name.upper()}
    if not rm:
        return m, {}
    cols = tuple(
        c.replace(name=rm[c.name]) if c.name in rm else c
        for c in m.datainfo
    )
    new_di = m.datainfo.replace(columns=cols)
    if m.dataset is not None:
        new_ds = m.dataset.rename(columns=rm)
        return m.replace(datainfo=new_di, dataset=new_ds), rm
    return m.replace(datainfo=new_di), rm
")
  result <- reticulate::py$`_pharmr_extra_uppercase_model`(model)
  list(
    model = result[[1]],
    renamed = unlist(result[[2]])
  )
}
