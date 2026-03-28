# Tests for create_sim_dataset()
#
# Tests are split into:
#   - Pure input-validation tests (no Pharmpy required)
#   - Functional tests (require Pharmpy, use skip_if_nonmem_not_available())
#
# Most functional tests pass `data = dat` directly so they never touch
# model$dataset and work with the in-memory model returned by
# make_model_without_cov() / pharmr::load_example_model().

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

## Single-subject dataset with CMT, EVID, MDV present
.one_subject_dat <- function() {
  data.frame(
    ID   = 1L,
    TIME = c(0, 6, 12, 24),
    DV   = c(0, 8, 5, 2),
    AMT  = c(100, 0, 0, 0),
    EVID = c(1L, 0L, 0L, 0L),
    MDV  = c(1L, 0L, 0L, 0L)
  )
}

## Multi-subject dataset (n_ids subjects, same profile)
.multi_subject_dat <- function(n_ids = 4) {
  lapply(seq_len(n_ids), function(i) {
    data.frame(
      ID   = i,
      TIME = c(0, 6, 12, 24),
      DV   = c(0, 8, 5, 2),
      AMT  = c(100, 0, 0, 0),
      EVID = c(1L, 0L, 0L, 0L),
      MDV  = c(1L, 0L, 0L, 0L)
    )
  }) |> dplyr::bind_rows()
}

# ===========================================================================
# Input validation — no Pharmpy required
# ===========================================================================

test_that("create_sim_dataset: error when model is not a Pharmpy object or string", {
  expect_error(
    create_sim_dataset(model = 42),
    "Pharmpy model object or a path"
  )
})

test_that("create_sim_dataset: error when model file does not exist", {
  expect_error(
    create_sim_dataset(model = "nonexistent.mod"),
    "does not exist"
  )
})

# ===========================================================================
# Return structure
# ===========================================================================

test_that("create_sim_dataset: returns a data.frame", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(model = mod, data = .one_subject_dat(), verbose = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("create_sim_dataset: always includes .regimen column", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(model = mod, data = .one_subject_dat(), verbose = FALSE)
  expect_true(".regimen" %in% names(out))
})

test_that("create_sim_dataset: .regimen is 'original regimens' when no regimen specified", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(model = mod, data = .one_subject_dat(), verbose = FALSE)
  expect_equal(unique(out$.regimen), "original regimens")
})

# ===========================================================================
# CMT column handling
# ===========================================================================

test_that("create_sim_dataset: CMT absent from input stays absent from output", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  dat <- data.frame(
    ID   = 1L, TIME = c(0, 12), DV = c(0, 5),
    AMT  = c(100, 0), EVID = c(1L, 0L), MDV = c(1L, 0L)
  )
  out <- create_sim_dataset(model = mod, data = dat, verbose = FALSE)
  expect_false("CMT" %in% names(out))
})

# ===========================================================================
# n_subjects
# ===========================================================================

test_that("create_sim_dataset: n_subjects truncates to the requested number", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(
    model = mod, 
    data = .multi_subject_dat(n_ids = 4),
    n_subjects = 2, verbose = FALSE
  )
  expect_equal(length(unique(out$ID)), 2)
})

test_that("create_sim_dataset: without n_subjects all subjects from data are kept", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(
    model = mod, data = .multi_subject_dat(n_ids = 3), verbose = FALSE
  )
  expect_equal(length(unique(out$ID)), 3)
})

# ===========================================================================
# data argument (override model$dataset)
# ===========================================================================

test_that("create_sim_dataset: data.frame override is used instead of model$dataset", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  dat <- .multi_subject_dat(n_ids = 2)
  out <- create_sim_dataset(model = mod, data = dat, verbose = FALSE)
  expect_equal(sort(unique(out$ID)), 1:2)
})

test_that("create_sim_dataset: error when data file path does not exist", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  expect_error(
    create_sim_dataset(model = mod, data = "no_such_file.csv", verbose = FALSE),
    "File not found"
  )
})

# ===========================================================================
# t_obs
# ===========================================================================

test_that("create_sim_dataset: obs records are only at t_obs times", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  t_obs <- c(2, 8, 24)
  out <- create_sim_dataset(
    model = mod, data = .one_subject_dat(),
    t_obs = t_obs, verbose = FALSE
  )
  obs_times <- out$TIME[out$EVID == 0]
  expect_true(all(obs_times %in% t_obs))
})

test_that("create_sim_dataset: dose records are preserved when only t_obs is set", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(
    model = mod, 
    data = .one_subject_dat(),
    t_obs = c(6, 12), verbose = FALSE
  )
  expect_true(any(out$EVID == 1))
  expect_equal(unique(out$AMT[out$EVID == 1]), 100)
})

test_that("create_sim_dataset: t_obs creates one obs row per subject per time", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod  <- make_model_without_cov()
  t_obs <- c(6, 12, 24)
  out  <- create_sim_dataset(
    model = mod, 
    data = .multi_subject_dat(n_ids = 3),
    t_obs = t_obs, verbose = FALSE
  )
  obs <- out[out$EVID == 0, ]
  expect_equal(nrow(obs), length(unique(out$ID)) * length(t_obs))
})

# ===========================================================================
# regimen
# ===========================================================================

test_that("create_sim_dataset: regimen as list replaces dose AMT", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  out <- create_sim_dataset(
    model = mod, data = .one_subject_dat(),
    regimen = list(dose = 250, interval = 12, n = 2, route = "iv"),
    verbose = FALSE
  )
  expect_equal(unique(out$AMT[out$EVID == 1]), 250)
})

test_that("create_sim_dataset: regimen as data.frame works and sets .regimen label", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod    <- make_model_without_cov()
  reg_df <- create_regimen(dose = 300, interval = 12, n = 2, route = "iv") |>
    dplyr::mutate(regimen = "300mg")
  out <- create_sim_dataset(
    model = mod, data = .one_subject_dat(),
    regimen = reg_df, verbose = FALSE
  )
  expect_equal(unique(out$.regimen), "300mg")
  expect_equal(unique(out$AMT[out$EVID == 1]), 300)
})

test_that("create_sim_dataset: multiple regimens produce multiple .regimen values", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  reg <- dplyr::bind_rows(
    create_regimen(dose = 100, interval = 12, n = 2, route = "iv") |>
      dplyr::mutate(regimen = "low"),
    create_regimen(dose = 200, interval = 12, n = 2, route = "iv") |>
      dplyr::mutate(regimen = "high")
  )
  out <- create_sim_dataset(
    model = mod, data = .one_subject_dat(),
    regimen = reg, verbose = FALSE
  )
  expect_setequal(unique(out$.regimen), c("low", "high"))
})

test_that("create_sim_dataset: error when regimen is neither list nor data.frame", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- make_model_without_cov()
  expect_error(
    create_sim_dataset(
      model = mod, 
      data = .one_subject_dat(),
      regimen = "bad_input", verbose = FALSE
    ),
    "data.frame or a list"
  )
})

# ===========================================================================
# covariates  (requires pharmr::get_model_covariates — uses pheno model)
# ===========================================================================

test_that("create_sim_dataset: covariate values are substituted per subject", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod  <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(50, 100), APGR = c(6, 8))
  out  <- create_sim_dataset(
    model = mod,
    covariates = covs,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 12),
    verbose = FALSE
  )
  expect_equal(unique(out$WGT[out$ID == 1]), 50)
  expect_equal(unique(out$WGT[out$ID == 2]), 100)
})

test_that("create_sim_dataset: covariates without ID generates sequential IDs", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod  <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(1.5, 2.0, 2.5), APGR = c(7, 5, 9))
  out  <- create_sim_dataset(
    model = mod,
    covariates = covs,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 12),
    verbose = FALSE
  )
  expect_equal(sort(unique(out$ID)), 1:3)
})

test_that("create_sim_dataset: n_subjects from nrow(covariates) when not specified", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod  <- pharmr::load_example_model("pheno")
  covs <- data.frame(WGT = c(1.5, 2.0), APGR = c(7, 5))
  out  <- create_sim_dataset(
    model = mod,
    covariates = covs,
    regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
    t_obs = seq(0, 36, 12),
    verbose = FALSE
  )
  expect_equal(length(unique(out$ID)), 2)
})

test_that("create_sim_dataset: error when required covariates are missing", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  mod <- pharmr::load_example_model("pheno")
  expect_error(
    create_sim_dataset(
      model = mod,
      covariates = data.frame(WRONG_COL = c(70, 85)),
      regimen = list(dose = 25, interval = 12, n = 3, route = "iv"),
      verbose = FALSE
    ),
    "Not all required covariates"
  )
})
