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
  expect_equal(dim(out), c(744, 9))
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
  expect_equal(dim(out), c(744, 9))
  unlink(tmp_mod)
})

# ---------------------------------------------------------------------------
# create_sim_dataset() + run_sim() integration tests
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

test_that("create_sim_dataset + run_sim: regimen + t_obs produces 1-subject output by default", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 100, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    verbose = FALSE
  )
  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_equal(length(unique(out$ID)), 1)
  expect_true(all(c("ID", "TIME", "DV", "IPRED") %in% names(out)))
})

test_that("create_sim_dataset: n_subjects controls number of simulated subjects", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 100, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    n_subjects = 8,
    verbose = FALSE
  )
  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$ID)), 8)
})

test_that("create_sim_dataset + run_sim: covariates determines n_subjects and appears in output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(1.5, 2, 2.5), APGR = c(7, 5, 9))

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    covariates = covs,
    verbose = FALSE
  )
  out <- run_sim(id = "sim1", model = mod, data = sim_dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(length(unique(out$ID)), 3)
  expect_true("WGT" %in% names(out))
  expect_true("APGR" %in% names(out))
})

test_that("create_sim_dataset + run_sim: covariates values are correctly carried into output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(50, 100), APGR = c(6, 8))

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 12),
    covariates = covs,
    verbose = FALSE
  )
  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  wgt_id1 <- unique(out$WGT[out$ID == 1])
  wgt_id2 <- unique(out$WGT[out$ID == 2])
  expect_equal(wgt_id1, 50)
  expect_equal(wgt_id2, 100)
})

test_that("create_sim_dataset + run_sim: multiple regimens produce separate regimen_label values", {
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

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = reg,
    t_obs = seq(0, 36, 6),
    n_subjects = 3,
    verbose = FALSE
  )
  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_setequal(unique(out$regimen_label), c("100mg", "200mg"))
  expect_equal(length(unique(out$ID[out$regimen_label == "100mg"])), 3)
  expect_equal(length(unique(out$ID[out$regimen_label == "200mg"])), 3)
})

test_that("create_sim_dataset: error when required covariate missing from covariates arg", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")

  expect_error(
    create_sim_dataset(
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

test_that("run_sim: error when data is a file path instead of a data.frame", {
  skip_if_nonmem_not_available()
  mod <- make_model_without_cov()
  expect_error(
    run_sim(model = mod, data = "some_file.csv"),
    "must be a data.frame"
  )
})

test_that("run_sim: error when tool/model engine is mismatched", {
  ## Asking for an nlmixr2 simulation on a NONMEM model object now reaches
  ## the nlmixr2 dispatch, which fails when it can't parse a function out
  ## of the NONMEM control stream.
  mod <- pharmr::load_example_model("pheno")
  expect_error(
    run_sim(model = mod, tool = "nlmixr2"),
    "Could not extract an nlmixr2 model function"
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

test_that("calc_pk_variables: warns and skips AUC_SS when last dose is '.' (NONMEM missing)", {
  dat <- dplyr::mutate(.make_pk_data(), CL = 5)
  reg <- data.frame(
    time = c(0, 12), dose = c("100", "."), route = "iv", regimen = "100mg",
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- calc_pk_variables(dat, regimen = reg),
    "Could not calculate AUCss"
  )
  expect_false("AUC_SS" %in% names(out))
})

test_that("calc_pk_variables: AUC_SS computed when dose is numeric-like character", {
  dat <- dplyr::mutate(.make_pk_data(), CL = 5)
  reg <- data.frame(
    time = c(0, 12), dose = c("100", "100"), route = "iv", regimen = "100mg",
    stringsAsFactors = FALSE
  )
  out <- calc_pk_variables(dat, regimen = reg)
  expect_true("AUC_SS" %in% names(out))
  expect_equal(unique(out$AUC_SS), 100 / 5)
})

# ── create_dosing_records() ─────────────────────────────────────────────────

.dose_data2 <- function() data.frame(ID = 1:2, TIME = 0, DV = 0, EVID = 1)

test_that("create_dosing_records: oral route assigns CMT = 1", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 2, advan = 2)
  expect_true(all(out$CMT == 1))
})

test_that("create_dosing_records: iv route assigns CMT = 2 for ADVAN2", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "iv") |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 2, advan = 2)
  expect_true(all(out$CMT == 2))
})

test_that("create_dosing_records: iv route uses CMT = 1 for ADVAN1 (no depot)", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "iv") |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1, advan = 1)
  expect_true(all(out$CMT == 1))
})

test_that("create_dosing_records: ADVAN1 with oral route errors", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  expect_error(
    create_dosing_records(reg, .dose_data2(), n_subjects = 1, advan = 1),
    "does not support oral"
  )
})

test_that("create_dosing_records: t_inf > 0 gives RATE = AMT / t_inf", {
  reg <- create_regimen(dose = 120, interval = 12, n = 2, route = "iv",
                        t_inf = 2) |>
    dplyr::mutate(regimen = "iv")
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1, advan = 2)
  expect_equal(unique(out$RATE[out$RATE > 0]), 60)  # 120 / 2
})

test_that("create_dosing_records: n_subjects > data IDs extends ID list", {
  reg <- create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
    dplyr::mutate(regimen = "oral")
  single_id <- data.frame(ID = 1, TIME = 0, DV = 0, EVID = 0)
  out <- create_dosing_records(reg, single_id, n_subjects = 3, advan = 2)
  expect_equal(sort(unique(out$ID)), 1:3)
})

test_that("create_dosing_records: multiple regimens propagate to .regimen column", {
  reg <- dplyr::bind_rows(
    create_regimen(dose = 100, interval = 12, n = 2, route = "oral") |>
      dplyr::mutate(regimen = "low"),
    create_regimen(dose = 200, interval = 12, n = 2, route = "oral") |>
      dplyr::mutate(regimen = "high")
  )
  out <- create_dosing_records(reg, .dose_data2(), n_subjects = 1, advan = 2)
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

test_that("run_sim (stub): add_pk_variables=TRUE computes AUC_SS when CL in output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(), # mock result includes CL = 2
    .package = "pharmr.extra"
  )
  ## .sim_dat() has AMT = 100 for the dose row (EVID = 1)
  out <- run_sim(model = mod, data = .sim_dat(), add_pk_variables = TRUE,
                 verbose = FALSE)
  expect_true("AUC_SS" %in% names(out))
  expect_equal(unique(out$AUC_SS), 100 / 2) # last dose / CL
})

test_that("sample_uncertainty_parameters draws from a real covariance matrix", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- pharmr::load_example_model("pheno")
  res <- pharmr::load_example_modelfit_results("pheno")
  draws <- sample_uncertainty_parameters(
    mod, res$parameter_estimates, res$covariance_matrix, n = 4, seed = 1
  )
  expect_s3_class(draws, "data.frame")
  expect_equal(nrow(draws), 4)
  ## columns follow the covariance matrix parameters
  expect_true(all(c("POP_CL", "IIV_CL") %in% names(draws)))
})

test_that("sample_uncertainty_parameters is invariant to covariance row order", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  fx  <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")
  cov <- as.matrix(fx$covariance_matrix)

  ## Permute rows only (columns intact). A correct impl realigns rows to
  ## columns, so sampling is identical to the unpermuted covariance.
  cov_shuffled <- cov[rev(seq_len(nrow(cov))), , drop = FALSE]

  d1 <- sample_uncertainty_parameters(
    mod, fx$parameter_estimates, cov, n = 3, seed = 7
  )
  d2 <- sample_uncertainty_parameters(
    mod, fx$parameter_estimates, cov_shuffled, n = 3, seed = 7
  )
  expect_equal(d1, d2)
})

test_that("sample_uncertainty_parameters rejects mismatched covariance row/col names", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  fx  <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")
  bad <- as.matrix(fx$covariance_matrix)
  rownames(bad)[1] <- "NOT_A_PARAM"

  expect_error(
    sample_uncertainty_parameters(mod, fx$parameter_estimates, bad, n = 2, seed = 1),
    "same parameters"
  )
})

test_that("sample_uncertainty_parameters draws from a real nlmixr2 FOCEi covariance", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  ## Anchor: covariance + estimates captured from an actual nlmixr2 FOCEi fit
  ## of the pheno example (see tests/testthat/fixtures/). The nlmixr2 $cov
  ## spans only the fixed effects (POP_CL, POP_VC, COVAPGR), not SIGMA/IIV, so
  ## the means are restricted to those parameters.
  fx  <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")

  draws <- sample_uncertainty_parameters(
    mod, fx$parameter_estimates, fx$covariance_matrix, n = 3, seed = 42
  )

  expect_s3_class(draws, "data.frame")
  expect_equal(nrow(draws), 3)
  ## restricted to the covariance parameters only
  expect_equal(sort(names(draws)), c("COVAPGR", "POP_CL", "POP_VC"))
  ## deterministic given the seed (numpy Generator is stable across versions)
  expect_equal(draws$POP_CL,
               c(0.004151587, 0.004306600, 0.004108466), tolerance = 1e-6)
  ## reproducible: same seed -> identical draws
  draws2 <- sample_uncertainty_parameters(
    mod, fx$parameter_estimates, fx$covariance_matrix, n = 3, seed = 42
  )
  expect_equal(draws, draws2)
})

test_that("run_sim (nlmixr2): n_uncertainty propagates a real nlmixr2 covariance", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_not_installed("rxode2")

  ## resolve fixture path before switching working directory
  fx  <- readRDS(test_path("fixtures", "nlmixr2_pheno_focei_fit.rds"))
  withr::local_dir(tempdir())

  mod <- pharmr::convert_model(pharmr::load_example_model("pheno"), "nlmixr")
  fit <- list(
    parameter_estimates = fx$parameter_estimates,
    covariance_matrix   = fx$covariance_matrix
  )
  dat <- as.data.frame(mod$dataset)
  dat$EVID <- ifelse(dat$AMT > 0, 1, 0)
  dat$MDV  <- ifelse(dat$DV == 0, 1, 0)

  ## nlmixr2's covariance omits SIGMA/IIV, so run_sim warns those are held fixed
  expect_warning(
    out <- run_sim(fit = fit, model = mod, data = dat, tool = "nlmixr2",
                   n_uncertainty = 3, n_iterations = 2, verbose = FALSE),
    "held at point estimate"
  )

  expect_s3_class(out, "data.frame")
  expect_true(".uncertainty" %in% names(out))
  expect_equal(sort(unique(out$.uncertainty)), 1:3)
})

test_that("run_sim: warns which parameters are held fixed under n_uncertainty", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  ## covariance covers only POP_CL; POP_V is estimated but uncovered -> warned
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = matrix(1, dimnames = list("POP_CL", "POP_CL"))
  )
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        data.frame(POP_CL = seq_len(n))   # only POP_CL sampled
      },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )

  expect_warning(
    run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
            n_uncertainty = 2, verbose = FALSE),
    "POP_V"
  )
})

test_that("run_sim: n_uncertainty without a fit covariance matrix errors", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  expect_error(
    run_sim(model = mod, data = .sim_dat(), n_uncertainty = 5, verbose = FALSE),
    "covariance matrix"
  )
})

test_that("run_sim (stub): n_uncertainty samples draws and tags .uncertainty column", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  n_draws <- 3L
  sampled <- 0L
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        sampled <<- n
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr"
  )

  out <- run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
                 n_uncertainty = n_draws, verbose = FALSE)

  expect_equal(sampled, n_draws)
  expect_true(".uncertainty" %in% names(out))
  expect_equal(sort(unique(out$.uncertainty)), seq_len(n_draws))
})

test_that("run_sim (stub): each replicate perturbs the model with its own draw", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  fake_fit <- list(
    parameter_estimates = c(POP_CL = 1, POP_V = 10),
    covariance_matrix   = diag(2)
  )
  n_draws <- 3L
  ## Distinct value per draw so we can verify draw r reaches replicate r.
  draws <- data.frame(POP_CL = c(11, 22, 33), POP_V = c(44, 55, 66))
  applied <- list()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        draws[seq_len(n), , drop = FALSE]
      },
    .package = "pharmr.extra"
  )
  local_mocked_bindings(
    set_initial_estimates = function(model, inits) {
      applied[[length(applied) + 1]] <<- inits
      model
    },
    .package = "pharmr"
  )

  out <- run_sim(fit = fake_fit, model = mod, data = .sim_dat(),
                 n_uncertainty = n_draws, verbose = FALSE)

  ## Draws must reach set_initial_estimates once per replicate, in order, each
  ## carrying that replicate's own row -- guards against a regression where the
  ## sampled parameters never reach the simulator.
  expect_length(applied, n_draws)
  for(r in seq_len(n_draws)) {
    expect_equal(applied[[r]], as.list(draws[r, , drop = FALSE]))
  }
})

test_that("run_sim (stub): n_uncertainty=0 behaves like no uncertainty (point estimate)", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- make_model_without_cov()
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = .sim_dat(), n_uncertainty = 0,
                 verbose = FALSE)
  expect_false(".uncertainty" %in% names(out))
})

test_that("create_sim_dataset: t_obs limits observation records to requested times", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  sim_dat <- create_sim_dataset(model = mod, t_obs = c(6, 12), verbose = FALSE)
  obs_rows <- sim_dat[sim_dat$EVID == 0, ]
  expect_true(all(obs_rows$TIME %in% c(6, 12)))
})

test_that("run_sim (stub): NULL table in results aborts with a NONMEM-failure error", {
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
  ## run_sim guards against silent NONMEM failures: when the simulation
  ## produces no output table it calls abort_on_failed_sim(), which errors
  ## (surfacing the .lst) rather than returning an empty table with a warning.
  expect_error(
    run_sim(model = mod, data = .sim_dat(), verbose = FALSE),
    "produced no output"
  )
})

test_that("create_sim_dataset: n_subjects truncates subjects from original dataset", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  sim_dat <- create_sim_dataset(model = mod, n_subjects = 1, verbose = FALSE)
  expect_equal(length(unique(sim_dat$ID)), 1)
})

test_that("run_sim (stub): regimen as data.frame (not list) works", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- .make_iv_model()
  reg_df <- create_regimen(dose = 100, interval = 12, n = 3, route = "iv") |>
    dplyr::mutate(regimen = "100mg_iv")
  sim_dat <- create_sim_dataset(
    model = mod, regimen = reg_df, t_obs = c(6, 12), n_subjects = 1,
    verbose = FALSE
  )
  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_result(),
    .package = "pharmr.extra"
  )
  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(unique(out$regimen_label), "100mg_iv")
})

# ===========================================================================
# covariates without ID column
# ===========================================================================

## Helper: mock result with n subjects, for covariate tests
.mock_nlme_n <- function(n) {
  tab <- lapply(seq_len(n), function(i) {
    data.frame(ID = i, TIME = c(0, 6, 12), DV = c(0, 5, 3),
               EVID = c(1L, 0L, 0L), PRED = 0, WGT = 1, APGR = 5)
  }) |> dplyr::bind_rows()
  result <- list()
  attr(result, "tables") <- list(simtab = tab)
  result
}

test_that("run_sim (stub): covariates without ID generates IDs 1:n", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs_no_id <- data.frame(WGT = c(1.5, 2.0, 2.5), APGR = c(7, 5, 9))

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    covariates = covs_no_id,
    verbose = FALSE
  )

  ## Capture the dataset written for NONMEM so we can inspect IDs
  captured_sim_data <- NULL
  local_mocked_bindings(
    run_nlme = function(data, ...) {
      captured_sim_data <<- utils::read.csv(data)
      .mock_nlme_n(3)
    },
    .package = "pharmr.extra"
  )

  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  ## Correct number of subjects
  expect_equal(length(unique(out$ID)), 3)
  ## IDs in the dataset sent to NONMEM are sequential integers
  expect_equal(sort(unique(captured_sim_data$ID)), 1:3)
})

test_that("run_sim (stub): covariates without ID infers n_subjects from nrow(covariates)", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs_no_id <- data.frame(WGT = c(1.5, 2.0), APGR = c(7, 5))  # 2 rows → 2 subjects

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    covariates = covs_no_id,
    verbose = FALSE
  )

  local_mocked_bindings(
    run_nlme = function(...) .mock_nlme_n(2),
    .package = "pharmr.extra"
  )

  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_equal(length(unique(out$ID)), 2)
})

test_that("run_sim (stub): covariates with ID column still works (regression)", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  covs_with_id <- data.frame(ID = c(10L, 20L), WGT = c(1.5, 2.0), APGR = c(7, 5))

  sim_dat <- create_sim_dataset(
    model = mod,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 6),
    covariates = covs_with_id,
    verbose = FALSE
  )

  captured_sim_data <- NULL
  local_mocked_bindings(
    run_nlme = function(data, ...) {
      captured_sim_data <<- utils::read.csv(data)
      .mock_nlme_n(2)
    },
    .package = "pharmr.extra"
  )

  out <- run_sim(model = mod, data = sim_dat, verbose = FALSE)

  expect_equal(length(unique(out$ID)), 2)
  ## IDs are re-indexed to 1:n (existing behaviour preserved)
  expect_equal(sort(unique(captured_sim_data$ID)), 1:2)
})

