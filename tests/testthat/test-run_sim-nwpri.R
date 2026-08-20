# Anchor: parameter-uncertainty sampling vs NONMEM $PRIOR NWPRI ----------------
#
# `run_sim(n_uncertainty = )` propagates parameter uncertainty by drawing
# parameter sets from the fit's covariance matrix and simulating once per draw.
# NONMEM does the same thing with `$PRIOR NWPRI` + `$SIMULATION TRUE=PRIOR`.
# These tests check the two agree.
#
# The reference draws come from an actual NONMEM 7.6.0 run; see
# `fixtures/_create-nwpri-anchor.R` for how the fixture is built. Both sides
# draw 1000 parameter sets from the same fit (a 1-compartment oral model, 60
# subjects, FOCE INTER with a full `$COVARIANCE` step), so the comparison is on
# aggregates, not on individual draws: the two use different random number
# streams, and their sampling distributions are not identical by construction.
#
# Two differences are expected and are asserted explicitly below, so they stay
# visible rather than being absorbed into loose tolerances:
#
#   1. NWPRI draws OMEGA and SIGMA from inverse-Wishart distributions, which are
#      right-skewed. Pharmpy draws every parameter from one truncated
#      multivariate normal, which is symmetric. Means and SDs still line up to
#      within a few percent; the shapes differ.
#   2. NWPRI treats the THETA prior, each OMEGA block and the SIGMA prior as
#      independent, so it discards the THETA-OMEGA and THETA-SIGMA covariances
#      that `$COVARIANCE` reports. Pharmpy samples all parameters jointly and
#      therefore reproduces those correlations.

.nwpri_anchor <- function() readRDS(test_path("fixtures", "nwpri_anchor.rds"))

.nwpri_our_draws <- function(anchor, n = nrow(anchor$nwpri_draws)) {
  ## the model is only needed for parameter names and bounds; its dataset is
  ## never read, so reading from a string avoids shipping the CSV
  model <- pharmr::read_model_from_string(anchor$model_code)
  draws <- sample_uncertainty_parameters(
    model               = model,
    parameter_estimates = anchor$parameter_estimates,
    covariance_matrix   = anchor$covariance_matrix,
    n                   = n,
    seed                = anchor$meta$seed
  )
  draws[, names(anchor$nwpri_draws), drop = FALSE]
}

.skewness <- function(x) mean((x - mean(x))^3) / stats::sd(x)^3

.rel_diff <- function(a, b) abs(a / b - 1)

test_that("uncertainty draws match NWPRI on the fixed effects", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)

  thetas <- c("POP_KA", "POP_CL", "POP_V")

  ## Both are multivariate normal around the point estimates with the estimated
  ## THETA covariance, so these should agree closely. Monte Carlo error on the
  ## mean is ~0.1% of the SD at n = 1000, so 1% is not a tight squeeze.
  for (p in thetas) {
    expect_lt(.rel_diff(mean(ours[[p]]), mean(nwpri[[p]])), 0.01)
    expect_lt(.rel_diff(stats::sd(ours[[p]]), stats::sd(nwpri[[p]])), 0.10)
  }

  ## and both centre on the point estimate
  for (p in thetas) {
    expect_lt(.rel_diff(mean(ours[[p]]), anchor$parameter_estimates[[p]]), 0.01)
  }
})

test_that("uncertainty draws match NWPRI on the variance parameters", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)

  variances <- c("IIV_CL", "IIV_V", "sigma_prop")

  ## Inverse-Wishart (NWPRI) vs multivariate normal (pharmpy). The first two
  ## moments still agree; the tolerances are wider than for the thetas because
  ## the distributions genuinely differ in shape, not because of noise.
  for (p in variances) {
    expect_lt(.rel_diff(mean(ours[[p]]), mean(nwpri[[p]])), 0.08)
    expect_lt(.rel_diff(stats::sd(ours[[p]]), stats::sd(nwpri[[p]])), 0.15)
  }
})

test_that("NWPRI's inverse-Wishart variance draws are skewed, ours are not", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)

  ## Documents difference (1): this is the one place the two samplers disagree
  ## by construction. If pharmpy ever switched to an inverse-Wishart prior for
  ## the variance parameters, this test should start failing.
  for (p in c("IIV_CL", "IIV_V", "sigma_prop")) {
    expect_gt(.skewness(nwpri[[p]]), 0.2)
    expect_lt(abs(.skewness(ours[[p]])), 0.2)
  }

  ## The thetas are normal on both sides. The bound is generous because the
  ## Monte Carlo standard error of a skewness estimate at n = 1000 is ~0.08.
  for (p in c("POP_KA", "POP_CL", "POP_V")) {
    expect_lt(abs(.skewness(nwpri[[p]])), 0.4)
    expect_lt(abs(.skewness(ours[[p]])), 0.4)
  }
})

test_that("uncertainty draws reproduce the THETA correlations NWPRI keeps", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)
  cov    <- anchor$covariance_matrix

  ## POP_KA and POP_V are correlated in the covariance matrix; both samplers
  ## carry that through, because it sits inside the THETA block.
  expected <- cov["POP_KA", "POP_V"] /
    sqrt(cov["POP_KA", "POP_KA"] * cov["POP_V", "POP_V"])
  expect_gt(expected, 0.2)

  r_nwpri <- stats::cor(nwpri$POP_KA, nwpri$POP_V)
  r_ours  <- stats::cor(ours$POP_KA, ours$POP_V)
  expect_lt(abs(r_nwpri - expected), 0.1)
  expect_lt(abs(r_ours - expected), 0.1)
  expect_lt(abs(r_ours - r_nwpri), 0.1)
})

test_that("uncertainty draws keep the THETA-OMEGA correlations NWPRI drops", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)
  cov    <- anchor$covariance_matrix

  ## Documents difference (2). POP_V and IIV_V are correlated in the reported
  ## covariance matrix. NWPRI puts them in separate, independent prior blocks
  ## and so samples them as uncorrelated; pharmpy samples jointly and keeps the
  ## correlation. Ours is the less lossy of the two.
  expected <- cov["POP_V", "IIV_V"] /
    sqrt(cov["POP_V", "POP_V"] * cov["IIV_V", "IIV_V"])
  expect_gt(expected, 0.15)

  expect_lt(abs(stats::cor(nwpri$POP_V, nwpri$IIV_V)), 0.1)
  expect_lt(abs(stats::cor(ours$POP_V, ours$IIV_V) - expected), 0.1)
})

test_that("the propagated prediction interval matches NWPRI's", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  anchor <- .nwpri_anchor()
  nwpri  <- anchor$nwpri_draws
  ours   <- .nwpri_our_draws(anchor)

  ## What run_sim(n_uncertainty = ) is actually for: the uncertainty band around
  ## the predicted profile. Rather than run 1000 NONMEM simulations on each
  ## side, evaluate the model's typical-value profile analytically per draw --
  ## both samplers feed the identical simulation machinery downstream, so any
  ## difference in the band comes from the parameter draws.
  tobs <- c(0.25, 0.5, 1, 2, 4, 6, 8, 12, 16, 24)
  dose <- 100
  profiles <- function(draws) {
    t(apply(draws, 1, function(p) {
      ke <- p[["POP_CL"]] / p[["POP_V"]]
      dose / p[["POP_V"]] * p[["POP_KA"]] / (p[["POP_KA"]] - ke) *
        (exp(-ke * tobs) - exp(-p[["POP_KA"]] * tobs))
    }))
  }
  p_nwpri <- profiles(nwpri)
  p_ours  <- profiles(ours)

  med_nwpri <- apply(p_nwpri, 2, stats::median)
  med_ours  <- apply(p_ours,  2, stats::median)
  expect_true(all(.rel_diff(med_ours, med_nwpri) < 0.01))

  ## width of the 90% uncertainty interval on the typical profile
  width <- function(m) {
    apply(m, 2, stats::quantile, 0.95) - apply(m, 2, stats::quantile, 0.05)
  }
  expect_true(all(.rel_diff(width(p_ours), width(p_nwpri)) < 0.15))
})
