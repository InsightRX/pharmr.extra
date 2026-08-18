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

test_that("repair_residuals leaves the fit untouched when tables are unusable", {
  local_pharmr.extra_options()
  model <- list(dataset = make_residual_test_dataset())
  fit <- list(residuals = data.frame(CWRES = 1))

  expect_equal(repair_residuals(fit, model, list()), fit)
  expect_equal(repair_residuals(fit, NULL, list()), fit)
  expect_null(repair_residuals(NULL, model, list()))
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
  py_res <- reticulate::py_get_attr(out, "residuals")
  py_index <- reticulate::py_to_r(reticulate::py_get_attr(py_res, "index"))
  expect_equal(as.numeric(py_index), rows - 1)
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
