make_residual_test_dataset <- function() {
  data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    EVID = c(1, 0, 0, 1, 0, 0),
    MDV = c(1, 0, 0, 1, 0, 0),
    DV = c(0, 1.2, 0.8, 0, 2.1, 1.4)
  )
}

test_that("build_keyed_residuals subsets a full-length table to observations", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  sdtab <- data.frame(
    ID = dataset$ID,
    TIME = dataset$TIME,
    PRED = seq_len(6),
    CWRES = c(0, 0.5, -0.3, 0, 1.1, -0.9),
    CIWRES = c(0, 0.4, -0.2, 0, 1.0, -0.8)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = sdtab))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), sum(dataset$MDV == 0))
  expect_equal(names(res), c("ROW", "ID", "TIME", "CWRES", "CIWRES"))
  expect_equal(res$ROW, c(2, 3, 5, 6))
  expect_equal(res$ID, c(1, 1, 2, 2))
  expect_equal(res$TIME, c(1, 2, 1, 2))
  expect_equal(res$CWRES, c(0.5, -0.3, 1.1, -0.9))
})

test_that("build_keyed_residuals keeps observations with all-zero residuals", {
  # Pharmpy drops these rows ((df != 0).any(axis=1)), which is what made
  # nrow(fit$residuals) disagree with the observation count (#120).
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  sdtab <- data.frame(
    ID = dataset$ID,
    TIME = dataset$TIME,
    CWRES = c(0, 0.5, 0, 0, 0, -0.9)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = sdtab))

  expect_equal(nrow(res), 4)
  expect_equal(res$CWRES, c(0.5, 0, 0, -0.9))
})

test_that("build_keyed_residuals accepts observation-only tables", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  obs_table <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(1, 2, 1, 2),
    IPRED = c(1, 2, 3, 4),
    IWRES = c(0.1, -0.2, 0.3, -0.4)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = obs_table))

  expect_equal(nrow(res), 4)
  expect_equal(res$ROW, c(2, 3, 5, 6))
  expect_equal(res$IWRES, c(0.1, -0.2, 0.3, -0.4))
})

test_that("build_keyed_residuals checks the key before binding positionally", {
  # as.data.frame(nlmixr2fit) returns rows in nlmixr2's ID-sorted order, which
  # need not be the dataset order.
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  dataset <- dataset[c(4, 5, 6, 1, 2, 3), ]  # subject 2 first
  rownames(dataset) <- NULL
  obs_table <- data.frame(  # still in ID order 1, 1, 2, 2
    ID = c(1, 1, 2, 2),
    TIME = c(1, 2, 1, 2),
    IWRES = c(0.1, -0.2, 0.3, -0.4)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = obs_table))

  # Fell through to key alignment rather than binding row-for-row.
  expect_equal(res$ID, c(2, 2, 1, 1))
  expect_equal(res$IWRES, c(0.3, -0.4, 0.1, -0.2))
})

test_that("build_keyed_residuals uses the last block of a stacked table", {
  # read_table_nm() skips only the first `TABLE NO.` header, so a multi-step
  # estimation (SAEM then IMP) comes back as several stacked copies.
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  saem <- data.frame(
    ID = dataset$ID, TIME = dataset$TIME,
    CWRES = c(0, 9, 9, 0, 9, 9)
  )
  imp <- data.frame(
    ID = dataset$ID, TIME = dataset$TIME,
    CWRES = c(0, 0.5, -0.3, 0, 1.1, -0.9)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = rbind(saem, imp)))

  expect_equal(nrow(res), 4)
  expect_equal(res$CWRES, c(0.5, -0.3, 1.1, -0.9))
})

test_that("build_keyed_residuals matches keys at NONMEM table precision", {
  # NONMEM writes tables at ~5 significant digits, so a dataset TIME of
  # 0.0833333 comes back as 8.33330E-02.
  local_pharmr.extra_options()
  dataset <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 0.0833333, 1.0166667),
    MDV = c(1, 0, 0)
  )
  tab <- data.frame(
    ID = c(1, 1),
    TIME = c(0.083333, 1.01667),
    CWRES = c(0.5, -0.3)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = tab))

  expect_equal(res$CWRES, c(0.5, -0.3))
})

test_that("build_keyed_residuals matches character IDs against numeric IDs", {
  local_pharmr.extra_options()
  dataset <- data.frame(
    ID = c("001", "001", "002"),
    TIME = c(0, 1, 1),
    MDV = c(1, 0, 0),
    stringsAsFactors = FALSE
  )
  tab <- data.frame(ID = c(1, 2), TIME = c(1, 1), CWRES = c(0.5, -0.3))

  res <- build_keyed_residuals(dataset, list(sdtab = tab))

  expect_equal(res$CWRES, c(0.5, -0.3))
})

test_that("build_keyed_residuals pairs repeated ID/TIME keys in order", {
  # Two analytes measured at the same nominal time: the key is duplicated, so
  # occurrences are paired up rather than all collapsing onto the first hit.
  local_pharmr.extra_options()
  dataset <- data.frame(
    ID = c(1, 1, 1, 2),
    TIME = c(0, 1, 1, 0),
    MDV = c(1, 0, 0, 1)
  )
  # Neither length branch applies, so this goes through key alignment.
  tab <- data.frame(
    ID = c(1, 1, 2),
    TIME = c(1, 1, 5),
    CWRES = c(0.5, -0.3, 9)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = tab))

  expect_equal(res$CWRES, c(0.5, -0.3))
})

test_that("build_keyed_residuals does not shadow an existing ROW column", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  dataset$ROW <- c(11, 12, 13, 14, 15, 16)  # not row numbers
  tab <- data.frame(
    ID = dataset$ID, TIME = dataset$TIME,
    CWRES = c(0, 0.5, -0.3, 0, 1.1, -0.9)
  )

  expect_warning(
    res <- build_keyed_residuals(dataset, list(sdtab = tab)),
    "already has a"
  )
  expect_equal(names(res), c(".ROW", "ID", "TIME", "CWRES"))
  expect_equal(res$.ROW, c(2, 3, 5, 6))
  expect_equal(attr(res, "row_col"), ".ROW")
})

test_that("build_keyed_residuals aligns on ID/TIME when row counts don't match", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  partial <- data.frame(
    ID = c(2, 1),
    TIME = c(2, 1),
    CWRES = c(-0.9, 0.5)
  )

  res <- build_keyed_residuals(dataset, list(sdtab = partial))

  expect_equal(nrow(res), 4)
  expect_equal(res$CWRES, c(0.5, NA, NA, -0.9))
})

test_that("build_keyed_residuals ignores tables from a different run", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  unrelated <- data.frame(
    ID = c(7, 8, 9),
    TIME = c(1, 2, 3),
    CWRES = c(0.1, 0.2, 0.3)
  )

  expect_null(build_keyed_residuals(dataset, list(sdtab = unrelated)))
})

test_that("build_keyed_residuals combines columns from multiple tables", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  tables <- list(
    sdtab = data.frame(
      ID = dataset$ID, TIME = dataset$TIME,
      CWRES = c(0, 0.5, -0.3, 0, 1.1, -0.9)
    ),
    restab = data.frame(
      ID = dataset$ID, TIME = dataset$TIME,
      IWRES = c(0, 0.1, -0.1, 0, 0.2, -0.2)
    )
  )

  res <- build_keyed_residuals(dataset, tables)

  expect_equal(names(res), c("ROW", "ID", "TIME", "CWRES", "IWRES"))
  expect_equal(res$IWRES, c(0.1, -0.1, 0.2, -0.2))
})

test_that("build_keyed_residuals returns NULL when nothing usable is found", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()

  expect_null(build_keyed_residuals(dataset, list()))
  expect_null(build_keyed_residuals(dataset, NULL))
  expect_null(build_keyed_residuals(NULL, list(sdtab = data.frame(CWRES = 1))))
  # table without residual columns:
  expect_null(
    build_keyed_residuals(
      dataset,
      list(patab = data.frame(ID = dataset$ID, CL = 1, V = 2))
    )
  )
  # table that can neither be length-matched nor key-matched:
  expect_null(
    build_keyed_residuals(dataset, list(sdtab = data.frame(CWRES = c(1, 2))))
  )
})

test_that("build_keyed_residuals falls back to EVID and to all rows", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  sdtab <- data.frame(CWRES = c(0, 0.5, -0.3, 0, 1.1, -0.9))

  no_mdv <- dataset[, setdiff(names(dataset), "MDV")]
  res_evid <- build_keyed_residuals(no_mdv, list(sdtab = sdtab))
  expect_equal(res_evid$ROW, c(2, 3, 5, 6))

  no_flags <- dataset[, c("ID", "TIME", "DV")]
  res_all <- build_keyed_residuals(no_flags, list(sdtab = sdtab))
  expect_equal(res_all$ROW, seq_len(6))
})

test_that("repair_residuals replaces the slot on an nlmixr2-shaped fit", {
  local_pharmr.extra_options()
  dataset <- make_residual_test_dataset()
  model <- list(dataset = dataset)
  fit <- structure(
    list(
      ofv = 1,
      residuals = data.frame(CWRES = c(0.5, -0.3, 1.1, -0.9))
    ),
    class = c("nlmixr2_modelfit_results", "list")
  )
  tables <- list(
    sdtab = data.frame(
      ID = c(1, 1, 2, 2),
      TIME = c(1, 2, 1, 2),
      CWRES = c(0.5, -0.3, 1.1, -0.9)
    )
  )

  out <- repair_residuals(fit, model, tables)

  expect_s3_class(out, "nlmixr2_modelfit_results")
  expect_equal(names(out$residuals), c("ROW", "ID", "TIME", "CWRES"))
  expect_equal(nrow(out$residuals), 4)
  expect_equal(out$ofv, 1)
})

test_that("repair_residuals keys against an explicit dataset when given", {
  # The nlmixr2 path fits `resolve_nlmixr_data(model, data)`, which is not
  # `model$dataset` when the caller passed an explicit `data =`.
  local_pharmr.extra_options()
  model <- list(dataset = make_residual_test_dataset())
  fit_data <- data.frame(
    ID = c(3, 3, 3),
    TIME = c(0, 1, 2),
    MDV = c(1, 0, 0),
    DV = c(0, 5, 6)
  )
  fit <- structure(
    list(residuals = data.frame(CWRES = c(1, 2))),
    class = c("nlmixr2_modelfit_results", "list")
  )
  tables <- list(
    sdtab = data.frame(ID = c(3, 3), TIME = c(1, 2), CWRES = c(0.7, -0.6))
  )

  out <- repair_residuals(fit, model, tables, dataset = fit_data)

  expect_equal(out$residuals$ID, c(3, 3))
  expect_equal(out$residuals$ROW, c(2, 3))
  expect_equal(out$residuals$CWRES, c(0.7, -0.6))
})

test_that("repair_residuals warns and leaves the fit untouched when tables are unusable", {
  local_pharmr.extra_options()
  model <- list(dataset = make_residual_test_dataset())
  fit <- list(residuals = data.frame(CWRES = 1))

  expect_warning(
    expect_equal(repair_residuals(fit, model, list()), fit),
    "Could not rebuild"
  )
  expect_equal(repair_residuals(fit, NULL, list()), fit)
  expect_null(repair_residuals(NULL, model, list()))
})

test_that("repair_residuals stays silent when there were no residuals to fix", {
  # e.g. an evaluation-only run.
  local_pharmr.extra_options()
  model <- list(dataset = make_residual_test_dataset())

  expect_no_warning(out <- repair_residuals(list(), model, list()))
  expect_equal(out, list())
  expect_no_warning(
    out2 <- repair_residuals(list(residuals = data.frame()), model, list())
  )
  expect_equal(out2, list(residuals = data.frame()))
})

test_that("set_pharmpy_residuals replaces the slot on a Pharmpy fit", {
  local_pharmr.extra_options()
  skip_if_not(
    reticulate::py_module_available("pharmpy"),
    "Pharmpy not available"
  )
  model <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  dataset <- model$dataset
  rows <- c(2, 3)
  res <- data.frame(
    ROW = rows,
    ID = dataset$ID[rows],
    TIME = dataset$TIME[rows],
    CWRES = c(0.1, -0.2)
  )

  out <- set_pharmpy_residuals(fit, model, res)

  expect_true(inherits(out, "python.builtin.object"))
  expect_equal(nrow(out$residuals), 2)
  expect_true(all(c("ROW", "ID", "TIME", "CWRES") %in% names(out$residuals)))
  expect_equal(out$residuals$CWRES, c(0.1, -0.2))
  # other slots are carried over unchanged:
  expect_equal(out$ofv, fit$ofv)
  expect_equal(out$parameter_estimates, fit$parameter_estimates)
  # the pandas index is set to the dataset row labels, which is what pharmpy
  # itself joins on:
  ## reticulate has no converter for a pandas Index — py_to_r() would hand
  ## back the Python object untouched — so read it out as a list. The
  ## expectation is derived from the dataset rather than assuming the index
  ## runs 0..n-1: what matters is that the labels are the ones the dataset
  ## carries at those row positions, whatever they are numbered from.
  py_index_of <- function(x) {
    idx <- reticulate::py_get_attr(x, "index")$tolist()
    if(inherits(idx, "python.builtin.object")) idx <- reticulate::py_to_r(idx)
    as.numeric(unlist(idx))
  }
  py_res <- reticulate::py_get_attr(out, "residuals")
  dataset_labels <- py_index_of(reticulate::py_get_attr(model, "dataset"))
  expect_equal(py_index_of(py_res), dataset_labels[rows])
})

test_that("set_pharmpy_residuals refuses to fall back to a default index", {
  # A RangeIndex would make pharmpy's `dataset.loc[residuals.index]` silently
  # select the first n dataset rows, dose records included.
  local_pharmr.extra_options()
  skip_if_not(
    reticulate::py_module_available("pharmpy"),
    "Pharmpy not available"
  )
  model <- pharmr::load_example_model("pheno")
  fit <- pharmr::load_example_modelfit_results("pheno")
  res <- data.frame(
    ROW = c(1, nrow(model$dataset) + 10),
    ID = c(1, 1),
    TIME = c(0, 1),
    CWRES = c(0.1, -0.2)
  )

  expect_error(set_pharmpy_residuals(fit, model, res), "outside the model dataset")
})

test_that("copy_fit_attributes carries user attributes onto the new object", {
  local_pharmr.extra_options()
  from <- structure(list(a = 1), class = "old_class", run = "run1", info = 2)
  to <- structure(list(b = 2), class = "new_class")

  out <- copy_fit_attributes(from, to)

  expect_equal(attr(out, "run"), "run1")
  expect_equal(attr(out, "info"), 2)
  expect_s3_class(out, "new_class")  # class is not overwritten
  expect_equal(names(out), "b")
})

test_that("dataset_key_columns falls back to ID/TIME", {
  local_pharmr.extra_options()
  expect_equal(dataset_key_columns(list()), list(id = "ID", idv = "TIME"))
  expect_equal(
    dataset_key_columns(
      list(datainfo = list(
        id_column = list(name = "SUBJ"),
        idv_column = list(name = "TAD")
      ))
    ),
    list(id = "SUBJ", idv = "TAD")
  )
})
