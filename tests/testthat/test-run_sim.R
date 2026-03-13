# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

skip_on_ci()

test_that("Basic simulation works (using `model` argument, not `fit`)", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
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
  expect_equal(dim(out), c(744, 10))
})

test_that("Basic simulation works (using model file specified to `model`)", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
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
  expect_equal(dim(out), c(744, 10))
  unlink(tmp_mod)
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
  skip_if_nonmem_not_available()
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
  skip_if_nonmem_not_available()
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
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(1.5, 2, 2.5), APGR = c(7, 5, 9))

  out <- run_sim(
    id = "sim1",
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
  skip_if_nonmem_not_available()
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
  skip_if_nonmem_not_available()
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
  withr::local_dir(tempdir())

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

test_that("run_sim: error when both fit and model are NULL", {
  expect_error(
    run_sim()
  )
})

test_that("run_sim: error when tool is not nonmem", {
  mod <- pharmr::load_example_model("pheno")
  expect_error(
    run_sim(model = mod, tool = "nlmixr2"),
    "currently only supporting NONMEM"
  )
})

# ===========================================================================
# Pure helper function tests — no NONMEM or Pharmpy required
# ===========================================================================

# ── fill_missing() ──────────────────────────────────────────────────────────

test_that("fill_missing: all-NA numeric returns all-zero", {
  expect_equal(fill_missing(c(NA_real_, NA_real_, NA_real_)), c(0, 0, 0))
})

test_that("fill_missing: all-NA character returns all '.'", {
  expect_equal(fill_missing(c(NA_character_, NA_character_)), c(".", "."))
})

test_that("fill_missing: vector with at least one non-NA passes through unchanged", {
  x <- c(1, NA, 3)
  expect_equal(fill_missing(x), x)
})

test_that("fill_missing: fully-populated vector passes through unchanged", {
  expect_equal(fill_missing(c(1, 2, 3)), c(1, 2, 3))
})

# ── match_type() ────────────────────────────────────────────────────────────

test_that("match_type: leaves AMT numeric when reference AMT is numeric", {
  x   <- data.frame(AMT = 1:3, RATE = 0.0, DV = 0.0)
  ref <- data.frame(AMT = 10:12, RATE = 0.0, DV = 0.0)
  out <- match_type(x, ref)
  expect_true(is.numeric(out$AMT))
})

test_that("match_type: coerces AMT to character when reference AMT is character", {
  x   <- data.frame(AMT = 1:3, RATE = 0.0, DV = 0.0)
  ref <- data.frame(AMT = c("100", "200", "300"), RATE = 0.0, DV = 0.0)
  out <- match_type(x, ref)
  expect_true(is.character(out$AMT))
  expect_equal(out$AMT, c("1", "2", "3"))
})

# ── calc_pk_variables() ─────────────────────────────────────────────────────

.make_pk_data <- function() {
  data.frame(
    ID   = c(rep(1L, 5), rep(2L, 5)),
    TIME = rep(c(0, 6, 12, 18, 24), 2),
    DV   = c(0, 10, 7, 4, 2,   # ID 1: Cmax = 10 @ t=6, Cmin = 2
             0,  8, 5, 3, 1),  # ID 2: Cmax =  8 @ t=6, Cmin = 1
    EVID = c(1, 0, 0, 0, 0,
             1, 0, 0, 0, 0)
  )
}

test_that("calc_pk_variables: returns NULL for NULL input", {
  expect_null(calc_pk_variables(NULL))
})

test_that("calc_pk_variables: CMAX_OBS and TMAX_OBS correct per ID", {
  out <- calc_pk_variables(.make_pk_data())
  expect_equal(unique(out$CMAX_OBS[out$ID == 1]), 10)
  expect_equal(unique(out$TMAX_OBS[out$ID == 1]), 6)
  expect_equal(unique(out$CMAX_OBS[out$ID == 2]), 8)
  expect_equal(unique(out$TMAX_OBS[out$ID == 2]), 6)
})

test_that("calc_pk_variables: CMIN_OBS computed correctly when EVID present", {
  out <- calc_pk_variables(.make_pk_data())
  expect_true("CMIN_OBS" %in% names(out))
  expect_equal(unique(out$CMIN_OBS[out$ID == 1]), 2)
  expect_equal(unique(out$CMIN_OBS[out$ID == 2]), 1)
})

test_that("calc_pk_variables: CMIN_OBS skipped with message when EVID absent", {
  dat <- dplyr::select(.make_pk_data(), -"EVID")
  expect_message(
    out <- calc_pk_variables(dat),
    "Skipping Cmin"
  )
  expect_false("CMIN_OBS" %in% names(out))
})

test_that("calc_pk_variables: AUC_SS added when regimen and CL both provided", {
  dat <- dplyr::mutate(.make_pk_data(), CL = 5)
  reg <- data.frame(time = c(0, 12), dose = c(100, 100), route = "iv",
                    regimen = "100mg")
  out <- calc_pk_variables(dat, regimen = reg)
  expect_true("AUC_SS" %in% names(out))
  expect_equal(unique(out$AUC_SS), 100 / 5)  # last dose / CL
})

test_that("calc_pk_variables: AUC_SS absent when CL not in data", {
  reg <- data.frame(time = 0, dose = 100, route = "iv", regimen = "100mg")
  out <- calc_pk_variables(.make_pk_data(), regimen = reg)
  expect_false("AUC_SS" %in% names(out))
})

test_that("calc_pk_variables: AUC_SS absent when regimen is NULL", {
  dat <- dplyr::mutate(.make_pk_data(), CL = 5)
  out <- calc_pk_variables(dat, regimen = NULL)
  expect_false("AUC_SS" %in% names(out))
})

# ── create_dosing_records() ─────────────────────────────────────────────────

.dose_data2 <- function() data.frame(ID = 1:2, TIME = 0, DV = 0, EVID = 1)
.dose_dict  <- function() list(ID = "ID", DV = "DV", EVID = "EVID",
                               AMT = "AMT", CMT = "CMT", MDV = "MDV")

test_that("create_dosing_records: oral route assigns CMT = 1", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 2,
                               dictionary = .dose_dict(), advan = 2)
  expect_true(all(out$CMT == 1))
})

test_that("create_dosing_records: iv route assigns CMT = 2 for ADVAN2", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "iv") |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 2,
                               dictionary = .dose_dict(), advan = 2)
  expect_true(all(out$CMT == 2))
})

test_that("create_dosing_records: iv route uses CMT = 1 for ADVAN1 (no depot)", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "iv") |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1,
                               dictionary = .dose_dict(), advan = 1)
  expect_true(all(out$CMT == 1))
})

test_that("create_dosing_records: ADVAN1 with oral route errors", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  expect_error(
    create_dosing_records(reg, .dose_data2(), n_subjects = 1,
                          dictionary = .dose_dict(), advan = 1),
    "does not support oral"
  )
})

test_that("create_dosing_records: t_inf > 0 gives RATE = AMT / t_inf", {
  reg <- create_regimen(dose = 120, interval = 12, n = 2, route = "iv",
                        t_inf = 2) |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1,
                               dictionary = .dose_dict(), advan = 2)
  expect_equal(unique(out$RATE[out$RATE > 0]), 60)  # 120 / 2
})

test_that("create_dosing_records: n_subjects > data IDs extends ID list", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  single_id <- data.frame(ID = 1, TIME = 0, DV = 0, EVID = 0)
  out <- create_dosing_records(reg, single_id, n_subjects = 3,
                               dictionary = .dose_dict(), advan = 2)
  expect_equal(sort(unique(out$ID)), 1:3)
})

test_that("create_dosing_records: multiple regimens propagate to .regimen column", {
  reg <- dplyr::bind_rows(
    create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
      dplyr::mutate(regimen = "low"),
    create_regimen(dose = 200, interval = 12, n = 2, route = "oral") |>
      dplyr::mutate(regimen = "high")
  )
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1,
                               dictionary = .dose_dict(), advan = 2)
  expect_setequal(unique(out$.regimen), c("low", "high"))
})

# ===========================================================================
# run_sim() with run_nlme() stubbed — exercises post-run_nlme code paths
# without requiring NONMEM to actually execute
# ===========================================================================

## Minimal fake run_nlme result
.mock_nlme_result <- function(tab = NULL) {
  if (is.null(tab)) {
    tab <- data.frame(
      ID   = c(1L, 1L, 1L),
      TIME = c(0, 6, 12),
      DV   = c(0, 5.1, 3.2),
      EVID = c(1L, 0L, 0L),
      PRED = c(0, 5.0, 3.0),
      CL   = c(2, 2, 2)
    )
  }
  result <- list()
  attr(result, "tables") <- list(simtab = tab)
  result
}

## Minimal dataset that satisfies run_sim() column expectations
.sim_dat <- function(n_ids = 1) {
  lapply(seq_len(n_ids), function(i) {
    data.frame(
      ID   = i,
      TIME = c(0, 6, 12),
      DV   = c(0, 5, 3),
      AMT  = c(100, 0, 0),
      EVID = c(1, 0, 0),
      MDV  = c(1, 0, 0)
    )
  }) |> dplyr::bind_rows()
}

test_that("run_sim (stub): update_table=FALSE skips variable setup, returns table", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), update_table = FALSE,
                 verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true("regimen_label" %in% names(out))
})

test_that("run_sim (stub): add_pk_variables=FALSE suppresses CMAX_OBS", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), add_pk_variables = FALSE,
                 verbose = FALSE)
  expect_false("CMAX_OBS" %in% names(out))
})

test_that("run_sim (stub): add_pk_variables=TRUE adds CMAX_OBS to output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), add_pk_variables = TRUE,
                 verbose = FALSE)
  expect_true("CMAX_OBS" %in% names(out))
})

test_that("run_sim (stub): t_obs filters output to only requested times", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), t_obs = c(6, 12),
                 verbose = FALSE)
  expect_true(all(out$TIME %in% c(6, 12)))
  expect_false(0 %in% out$TIME)
})

test_that("run_sim (stub): NULL table in results triggers warning and empty output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  empty_result <- list()
  attr(empty_result, "tables") <- list(simtab = NULL)
  local_mocked_bindings(
    run_nlme = function(...) empty_result,
    .package = "pharmr.extra"
  )
  expect_warning(
    out <- run_sim(model = mod, data = .sim_dat(), verbose = FALSE),
    "did not output any results"
  )
  expect_equal(nrow(out), 0)
})

test_that("run_sim (stub): n_subjects truncates subjects from original dataset", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  tab2 <- data.frame(
    ID = c(1L, 1L, 2L, 2L), TIME = c(0, 12, 0, 12),
    DV = c(0, 5, 0, 6), EVID = c(1L, 0L, 1L, 0L),
    PRED = 0, CL = 2
  )
  mock_res <- list()
  attr(mock_res, "tables") <- list(simtab = tab2)
  local_mocked_bindings(
    run_nlme = function(...) mock_res,
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(n_ids = 3), n_subjects = 2,
                 verbose = FALSE)
  expect_lte(length(unique(out$ID)), 2)
})

test_that("run_sim (stub): regimen as data.frame (not list) works", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  reg_df <- create_regimen(dose = 100, interval = 12, n = 3, route = "iv") |>
    dplyr::mutate(regimen = "100mg_iv")
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), regimen = reg_df,
                 t_obs = c(6, 12), n_subjects = 1, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(unique(out$regimen_label), "100mg_iv")
})

