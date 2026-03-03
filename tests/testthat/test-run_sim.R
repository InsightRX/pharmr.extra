# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

skip_on_ci()

test_that("Basic simulation works (using `model` argument, not `fit`)", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())
  
  mod <- pharmr::load_example_model("pheno")
  pharmr::load_dataset(mod)
  dat <- mod$dataset |>
    as.data.frame() |>
    dplyr::mutate(
      EVID = ifelse(AMT == 0, 0, 1),
      MDV = ifelse(DV == 0, 1, 0),
      CMT = 1
    )
  out <- run_sim(
    model = mod, 
    data = dat,
    variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED")
  )
  expect_equal(dim(out), c(744, 12))
})

test_that("Basic simulation works (using model file specified to `model`)", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  model_code <- mod$code
  pharmr::load_dataset(mod)
  dat <- mod$dataset |>
    as.data.frame() |>
    dplyr::mutate(
      EVID = ifelse(AMT == 0, 0, 1),
      MDV = ifelse(DV == 0, 1, 0),
      CMT = 1
    )
  # Write model code to a temp file (run_sim now expects a filename)
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)
  out <- run_sim(
    model = tmp_mod, # !! filename, not model object
    data = dat,
    variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED")
  )
  expect_equal(dim(out), c(744, 12))
  unlink(tmp_mod)
})

test_that("Errors on invalid variables when update_table = TRUE", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)

  expect_error(
    run_sim(
      model = mod,
      data = dat,
      variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED", "NOPE", "WRONG"),
      update_table = TRUE
    ),
    "NOPE and WRONG are not valid variables"
  )
})

# ---------------------------------------------------------------------------
# No-data mode: run_sim() with regimen + t_obs, no `data` argument
# ---------------------------------------------------------------------------

## Minimal covariate-free model used by several tests below
.make_iv_model <- function() {
  dat <- data.frame(
    ID = 1, TIME = c(0, 6, 12, 24),
    DV = c(0, 8, 5, 2), AMT = c(100, 0, 0, 0),
    CMT = 1, EVID = c(1, 0, 0, 0), MDV = c(1, 0, 0, 0)
  )
  create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
}

test_that("run_sim: no data, regimen + t_obs produces 1-subject output by default", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  out <- run_sim(
    model = mod,
    regimen = list(dose = 100, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_equal(length(unique(out$ID)), 1)
  expect_true(all(c("ID", "TIME", "DV", "IPRED") %in% names(out)))
})

test_that("run_sim: no data, n_subjects controls number of simulated subjects", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  out <- run_sim(
    model = mod,
    regimen = list(dose = 100, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    n_subjects = 8,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$ID)), 8)
})

test_that("run_sim: no data, covariates determines n_subjects and appears in output", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(70, 85, 60), APGR = c(7, 5, 9))

  out <- run_sim(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    covariates = covs,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$ID)), 3)
  expect_true("WGT" %in% names(out))
  expect_true("APGR" %in% names(out))
})

test_that("run_sim: no data, covariates values are correctly carried into output", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(50, 100), APGR = c(6, 8))

  out <- run_sim(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 12),
    covariates = covs,
    verbose = FALSE
  )

  wgt_id1 <- unique(out$WGT[out$ID == 1])
  wgt_id2 <- unique(out$WGT[out$ID == 2])
  expect_equal(wgt_id1, 50)
  expect_equal(wgt_id2, 100)
})

test_that("run_sim: no data, multiple regimens produce separate regimen_label values", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  reg <- dplyr::bind_rows(
    create_regimen(dose = 100, interval = 12, n = 3, route = "iv") |>
      dplyr::mutate(regimen = "100mg"),
    create_regimen(dose = 200, interval = 12, n = 3, route = "iv") |>
      dplyr::mutate(regimen = "200mg")
  )

  out <- run_sim(
    model = mod,
    regimen = reg,
    t_obs = seq(0, 36, 6),
    n_subjects = 3,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_setequal(unique(out$regimen_label), c("100mg", "200mg"))
  expect_equal(length(unique(out$ID[out$regimen_label == "100mg"])), 3)
  expect_equal(length(unique(out$ID[out$regimen_label == "200mg"])), 3)
})

test_that("run_sim: no data, error when required covariate missing from covariates arg", {
  local_pharmr.extra_options()

  mod <- pharmr::load_example_model("pheno")

  expect_error(
    run_sim(
      model = mod,
      regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
      t_obs = seq(0, 36, 6),
      covariates = data.frame(WRONG_COL = c(70, 85))
    ),
    "Not all required covariates"
  )
})
