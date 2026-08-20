# Create the generated-NWPRI anchor fixture -----------------------------------
#
# Builds `nwpri_generated_anchor.rds`, the reference used by
# `tests/testthat/test-add_nwpri_prior.R` to check that the control streams
# `add_nwpri_prior()` writes are (a) accepted by NONMEM and (b) sample the
# parameters they are supposed to sample.
#
# `nwpri_anchor.rds` (see `_create-nwpri-anchor.R`) answers a different
# question: it holds NWPRI draws from a *hand-written* control stream, and is
# what `run_sim(n_uncertainty = )`'s own sampler is compared against. This
# fixture holds draws from the control stream the package *generates*, so the
# two together pin down both ends: the generator reproduces the hand-written
# reference, and the hand-written reference matches the R sampler.
#
# The script needs NONMEM, so it does not run in CI. It is run by hand inside
# the `pmx` container, which ships NONMEM 7.6.0 and pharmpy:
#
#   docker run --rm \
#     -v "$PWD:/pkg" \
#     -e RETICULATE_PYTHON=/app/venv/bin/python \
#     -w /pkg/tests/testthat/fixtures --entrypoint sh pmx:latest \
#     -c 'Rscript _create-nwpri-generated-anchor.R'
#
# Everything is seeded, so re-running reproduces the same fixture (given the
# same NONMEM and pharmpy versions).
#
# Three cases are covered, chosen for the three ways the emitted records differ:
#
#   1. `diagonal`  - the `nwpri_anchor.rds` fit: three THETAs, two 1x1 OMEGA
#      blocks, one SIGMA. The plain case, and the one directly comparable to
#      the hand-written anchor.
#   2. `block`     - a `$OMEGA BLOCK(2)` plus a second `$SIGMA`, so `$OMEGAP`
#      has to be emitted as a BLOCK record and `$OMEGAPD` has to carry one
#      degrees-of-freedom value per block rather than per element.
#   3. `fixed`     - a FIXED THETA, i.e. a parameter the covariance matrix does
#      not cover. It still has to appear in `$THETAP`/`$THETAPV`, and must come
#      back out of the draws unchanged.

library(cli)
library(reticulate)

NONMEM  <- "/opt/NONMEM/nm_current/run/nmfe76"
N_DRAWS <- 1000L
SEED    <- 20260821L

stopifnot(file.exists(NONMEM))

fixtures <- normalizePath(".")
pkg_root <- normalizePath(file.path(fixtures, "..", "..", ".."))

# Load the package ------------------------------------------------------------
# The pmx container ships NONMEM and pharmpy but not the three InsightRX/CRAN
# packages pharmr.extra Imports (irxutils, job, PKPDsim), and none of them is
# needed for anything this script touches. Fall back to loading a copy with
# those declarations stripped rather than requiring a bespoke image.
load_pharmr_extra <- function(path) {
  ok <- tryCatch({
    pkgload::load_all(path, quiet = TRUE, helpers = FALSE,
                      attach_testthat = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (ok) return(invisible(TRUE))

  cli::cli_alert_info("Loading pharmr.extra without its optional Imports.")
  copy <- file.path(tempdir(), "pharmr.extra")
  unlink(copy, recursive = TRUE)
  dir.create(copy, recursive = TRUE)
  for (d in c("R", "man", "inst", "tests")) {
    if (dir.exists(file.path(path, d))) file.copy(file.path(path, d), copy, recursive = TRUE)
  }
  optional <- "irxutils|job|PKPDsim"
  writeLines(
    grep(paste0("^\\s+(", optional, "),?$"), readLines(file.path(path, "DESCRIPTION")),
         invert = TRUE, value = TRUE),
    file.path(copy, "DESCRIPTION")
  )
  writeLines(
    grep(optional, readLines(file.path(path, "NAMESPACE")), invert = TRUE, value = TRUE),
    file.path(copy, "NAMESPACE")
  )
  file.copy(file.path(path, "LICENSE"), copy)
  pkgload::load_all(copy, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
  invisible(TRUE)
}
load_pharmr_extra(pkg_root)

work <- file.path(tempdir(), "nwpri-generated")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
owd <- setwd(work)
on.exit(setwd(owd), add = TRUE)

run_nonmem <- function(mod, lst) {
  status <- system2(NONMEM, c(mod, lst), stdout = FALSE, stderr = FALSE)
  if (status != 0) {
    out <- if (file.exists(lst)) utils::tail(readLines(lst, warn = FALSE), 40) else character(0)
    cli::cli_abort(c(
      "NONMEM failed on {mod} (exit {status}).",
      i = "In {.path {getwd()}}",
      gsub("\\}", "}}", gsub("\\{", "{{", out))
    ))
  }
}

# Read the point estimates and covariance matrix of a finished NONMEM run,
# renaming NONMEM's parameter labels to the pharmpy names taken from the
# $THETA/$OMEGA/$SIGMA comments.
read_fit <- function(stem, pharmpy_names, drop = character(0)) {
  lst <- readLines(paste0(stem, ".lst"))
  if (!any(grepl("MINIMIZATION SUCCESSFUL", lst))) {
    cli::cli_abort("Estimation did not minimise successfully ({stem}).")
  }
  if (any(grepl("COVARIANCE MATRIX UNOBTAINABLE", lst))) {
    cli::cli_abort("Covariance step failed ({stem}).")
  }
  ext     <- readLines(paste0(stem, ".ext"))
  ext_hdr <- strsplit(trimws(ext[2]), "[ ]+")[[1]][-1]
  ext_row <- function(tag) {
    line <- grep(paste0("^ *", tag, " "), ext, value = TRUE)[1]
    stats::setNames(as.numeric(strsplit(trimws(line), "[ ]+")[[1]][-1]), ext_hdr)
  }
  estimates <- ext_row("-1000000000")
  estimates <- estimates[names(estimates) != "OBJ"]

  cov_lines <- readLines(paste0(stem, ".cov"))
  cov_names <- strsplit(trimws(cov_lines[2]), "[ ]+")[[1]][-1]
  cov_mat   <- do.call(rbind, lapply(
    strsplit(trimws(cov_lines[-(1:2)]), "[ ]+"),
    function(x) as.numeric(x[-1])
  ))
  dimnames(cov_mat) <- list(cov_names, cov_names)

  ## Structurally-fixed elements are reported as all-zero rows/columns, which
  ## are neither positive definite nor parameters as far as pharmpy is
  ## concerned.
  keep <- setdiff(cov_names, drop)
  cov_mat <- cov_mat[keep, keep, drop = FALSE]

  rename <- function(x) unname(pharmpy_names[x])
  names(estimates) <- rename(names(estimates))
  dimnames(cov_mat) <- list(rename(keep), rename(keep))
  estimates <- estimates[!is.na(names(estimates))]
  list(parameter_estimates = estimates,
       standard_errors     = stats::setNames(sqrt(diag(cov_mat)), colnames(cov_mat)),
       covariance_matrix   = cov_mat)
}

# Build a prior control stream with `add_nwpri_prior()` + `set_simulation_clean()`
# and run it, returning the sampled parameters.
#
# `table_vars` maps parameter name -> the plain variable the model assigns it to
# (`T1 = THETA(1)` and friends). The indirection is needed twice over: tabling
# THETA(1) directly is not possible, and naming the variable after the parameter
# would make pharmpy rename the parameter itself.
draw_from_prior <- function(name, model_code, fit, table_vars, n_draws = N_DRAWS) {
  cli::cli_alert_info("Case {.val {name}}: building prior and drawing {n_draws} parameter sets")

  model <- pharmr::read_model_from_string(model_code)
  model <- pharmr::set_initial_estimates(
    model,
    inits = as.list(fit$parameter_estimates[intersect(
      names(fit$parameter_estimates), model$parameters$names
    )])
  )
  ## The code the prior records are inserted into, and the parameter structure
  ## they are built from, are both frozen alongside the result: that lets the
  ## record construction be re-checked without pharmpy, against a control
  ## stream NONMEM is known to accept.
  input_code <- model$code
  param_structure <- pharmr.extra:::nwpri_model_structure(model)
  prior_model <- add_nwpri_prior(
    model,
    parameter_estimates = fit$parameter_estimates,
    covariance_matrix   = fit$covariance_matrix
  )
  generated_code <- prior_model$code
  sim_code <- set_simulation_clean(
    prior_model, seed = SEED, n = n_draws, true_prior = TRUE
  )
  sim_code <- paste0(
    sim_code, "\n\n$TABLE ", paste(unname(table_vars), collapse = " "),
    " FIRSTONLY NOAPPEND NOPRINT ONEHEADER FILE=priortab\n"
  )

  dir <- file.path(work, name)
  dir.create(dir, showWarnings = FALSE)
  writeLines(c("ID,TIME,DV,AMT,EVID,MDV,CMT",
               "1,0,0,100,1,1,1",
               "1,1,0,0,0,0,2"),
             file.path(dir, "prior_data.csv"))
  writeLines(change_nonmem_dataset(sim_code, "prior_data.csv"),
             file.path(dir, "prior.mod"))

  here <- setwd(dir)
  on.exit(setwd(here), add = TRUE)
  run_nonmem("prior.mod", "prior.lst")

  draws <- read_table_nm(file = "priortab", subproblems = TRUE)
  stopifnot(nrow(draws) == n_draws, max(draws$.subproblem) == n_draws)
  draws <- draws[, unname(table_vars), drop = FALSE]
  names(draws) <- names(table_vars)

  list(
    model_code          = model_code,
    input_code          = input_code,
    param_structure     = param_structure,
    generated_code      = generated_code,
    sim_code            = sim_code,
    parameter_estimates = fit$parameter_estimates,
    standard_errors     = fit$standard_errors,
    covariance_matrix   = fit$covariance_matrix,
    draws               = draws
  )
}

# 1. `diagonal`: the nwpri_anchor fit ------------------------------------------
# Same fit, same PLEV, same number of draws as `_create-nwpri-anchor.R`, so the
# generated stream can be held directly against the hand-written one.
anchor <- readRDS(file.path(fixtures, "nwpri_anchor.rds"))

diagonal_code <- '$PROBLEM NWPRI generated anchor: diagonal

$INPUT ID TIME DV AMT EVID MDV CMT
$DATA prior_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
T1 = THETA(1)
T2 = THETA(2)
T3 = THETA(3)
O11 = OMEGA(1,1)
O22 = OMEGA(2,2)

KA = THETA(1)
CL = THETA(2)*EXP(ETA(1))
V  = THETA(3)*EXP(ETA(2))
S2 = V

$ERROR
S11 = SIGMA(1,1)
IPRED = F
Y = IPRED*(1 + EPS(1))

$THETA (0, 1.0)  ; POP_KA
$THETA (0, 5.0)  ; POP_CL
$THETA (0, 50.0) ; POP_V
$OMEGA 0.09 ; IIV_CL
$OMEGA 0.09 ; IIV_V
$SIGMA 0.04 ; sigma_prop

$ESTIMATION METHOD=COND INTER MAXEVAL=9999 PRINT=5 NOABORT
$COVARIANCE UNCONDITIONAL PRINT=E
'

cases <- list()
cases$diagonal <- draw_from_prior(
  name       = "diagonal",
  model_code = diagonal_code,
  fit        = list(parameter_estimates = anchor$parameter_estimates,
                    standard_errors     = anchor$standard_errors,
                    covariance_matrix   = anchor$covariance_matrix),
  table_vars = c(POP_KA = "T1", POP_CL = "T2", POP_V = "T3",
                 IIV_CL = "O11", IIV_V = "O22", sigma_prop = "S11")
)

# 2. `block`: $OMEGA BLOCK(2) and two SIGMAs -----------------------------------
set.seed(SEED)
n_id  <- 80
dose  <- 100
tobs  <- c(0.25, 0.5, 1, 2, 4, 6, 8, 12, 16, 24)
ka    <- 1.0
cl    <- 5.0
vc    <- 50.0
om    <- matrix(c(0.09, 0.045, 0.045, 0.09), 2, 2)   # correlated IIV on CL and V
chol_om <- chol(om)

dat <- do.call(rbind, lapply(seq_len(n_id), function(i) {
  eta   <- as.numeric(matrix(stats::rnorm(2), nrow = 1) %*% chol_om)
  cl_i  <- cl * exp(eta[1])
  v_i   <- vc * exp(eta[2])
  ke    <- cl_i / v_i
  ipred <- dose / v_i * ka / (ka - ke) * (exp(-ke * tobs) - exp(-ka * tobs))
  dv    <- ipred * (1 + stats::rnorm(length(tobs), 0, 0.2)) +
    stats::rnorm(length(tobs), 0, 0.05)
  rbind(
    data.frame(ID = i, TIME = 0, DV = 0, AMT = dose, EVID = 1, MDV = 1, CMT = 1),
    data.frame(ID = i, TIME = tobs, DV = round(pmax(dv, 1e-4), 4), AMT = 0,
               EVID = 0, MDV = 0, CMT = 2)
  )
}))
write.csv(dat, file.path(work, "block_data.csv"), row.names = FALSE, quote = FALSE)

block_est_code <- '$PROBLEM NWPRI generated anchor: block, estimation

$INPUT ID TIME DV AMT EVID MDV CMT
$DATA block_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
KA = THETA(1)
CL = THETA(2)*EXP(ETA(1))
V  = THETA(3)*EXP(ETA(2))
S2 = V

$ERROR
IPRED = F
Y = IPRED*(1 + EPS(1)) + EPS(2)

$THETA (0, 1.0)  ; POP_KA
$THETA (0, 5.0)  ; POP_CL
$THETA (0, 50.0) ; POP_V
$OMEGA BLOCK(2)
0.09        ; IIV_CL
0.03 0.09   ; IIV_V
$SIGMA 0.04 ; sigma_prop
$SIGMA 0.0025 ; sigma_add

$ESTIMATION METHOD=COND INTER MAXEVAL=9999 PRINT=5 NOABORT
$COVARIANCE UNCONDITIONAL PRINT=E
'
writeLines(block_est_code, file.path(work, "block_est.mod"))
run_nonmem("block_est.mod", "block_est.lst")

block_fit <- read_fit(
  "block_est",
  pharmpy_names = c(
    "THETA1"     = "POP_KA",
    "THETA2"     = "POP_CL",
    "THETA3"     = "POP_V",
    "OMEGA(1,1)" = "IIV_CL",
    "OMEGA(2,1)" = "OMEGA_2_1",
    "OMEGA(2,2)" = "IIV_V",
    "SIGMA(1,1)" = "sigma_prop",
    "SIGMA(2,1)" = "SIGMA_2_1",
    "SIGMA(2,2)" = "sigma_add"
  ),
  drop = "SIGMA(2,1)"
)

block_code <- '$PROBLEM NWPRI generated anchor: block

$INPUT ID TIME DV AMT EVID MDV CMT
$DATA prior_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
T1 = THETA(1)
T2 = THETA(2)
T3 = THETA(3)
O11 = OMEGA(1,1)
O21 = OMEGA(2,1)
O22 = OMEGA(2,2)

KA = THETA(1)
CL = THETA(2)*EXP(ETA(1))
V  = THETA(3)*EXP(ETA(2))
S2 = V

$ERROR
S11 = SIGMA(1,1)
S22 = SIGMA(2,2)
IPRED = F
Y = IPRED*(1 + EPS(1)) + EPS(2)

$THETA (0, 1.0)  ; POP_KA
$THETA (0, 5.0)  ; POP_CL
$THETA (0, 50.0) ; POP_V
$OMEGA BLOCK(2)
0.09        ; IIV_CL
0.03 0.09   ; IIV_V
$SIGMA 0.04 ; sigma_prop
$SIGMA 0.0025 ; sigma_add

$ESTIMATION METHOD=COND INTER MAXEVAL=9999 PRINT=5 NOABORT
$COVARIANCE UNCONDITIONAL PRINT=E
'

cases$block <- draw_from_prior(
  name       = "block",
  model_code = block_code,
  fit        = block_fit,
  table_vars = c(POP_KA = "T1", POP_CL = "T2", POP_V = "T3",
                 IIV_CL = "O11", OMEGA_2_1 = "O21", IIV_V = "O22",
                 sigma_prop = "S11", sigma_add = "S22")
)

# 3. `fixed`: a THETA the covariance matrix does not cover ---------------------
# POP_KA is FIXED in the model and dropped from the covariance matrix, so
# `add_nwpri_prior()` has to emit it with a negligible prior variance. Its draws
# should come back at the point estimate.
fixed_code <- sub(
  "$THETA (0, 1.0)  ; POP_KA",
  "$THETA (0, 1.0) FIX ; POP_KA",
  diagonal_code,
  fixed = TRUE
)
fixed_code <- sub("diagonal", "fixed", fixed_code, fixed = TRUE)

keep_fixed <- setdiff(colnames(anchor$covariance_matrix), "POP_KA")
fixed_fit <- list(
  parameter_estimates = anchor$parameter_estimates,
  standard_errors     = anchor$standard_errors[keep_fixed],
  covariance_matrix   = anchor$covariance_matrix[keep_fixed, keep_fixed, drop = FALSE]
)

cases$fixed <- draw_from_prior(
  name       = "fixed",
  model_code = fixed_code,
  fit        = fixed_fit,
  table_vars = c(POP_KA = "T1", POP_CL = "T2", POP_V = "T3",
                 IIV_CL = "O11", IIV_V = "O22", sigma_prop = "S11")
)

# 4. write the fixture --------------------------------------------------------
setwd(fixtures)
saveRDS(
  list(
    cases = cases,
    meta = list(
      nonmem   = "7.6.0",
      pharmpy  = as.character(reticulate::import("pharmpy")$`__version__`),
      n_draws  = N_DRAWS,
      seed     = SEED,
      plev     = 0.9999,
      created  = Sys.Date()
    )
  ),
  file.path(fixtures, "nwpri_generated_anchor.rds"),
  version = 2
)
cli::cli_alert_success(
  "Wrote nwpri_generated_anchor.rds ({length(cases)} cases x {N_DRAWS} draws)."
)
