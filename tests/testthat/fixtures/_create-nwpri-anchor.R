# Create the NWPRI anchor fixture ---------------------------------------------
#
# Builds `nwpri_anchor.rds`, the reference used by
# `tests/testthat/test-run_sim-nwpri.R` to check that the parameter-uncertainty
# sampling behind `run_sim(n_uncertainty = )` agrees with NONMEM's own
# uncertainty-simulation routine, `$PRIOR NWPRI` + `$SIMULATION TRUE=PRIOR`.
#
# The script needs NONMEM, so it does not run in CI. It is run by hand inside
# the `pmx` container, which ships NONMEM 7.6.0 and pharmpy:
#
#   docker run --rm \
#     -v "$PWD/tests/testthat/fixtures:/fixtures" \
#     -v "$PWD:/pkg" \
#     -e RETICULATE_PYTHON=/app/venv/bin/python \
#     -w /fixtures --entrypoint sh pmx:latest \
#     -c 'Rscript _create-nwpri-anchor.R'
#
# Everything is seeded, so re-running reproduces the same fixture (given the
# same NONMEM version).
#
# Steps:
#   1. simulate a 1-compartment oral PK dataset
#   2. fit it in NONMEM with FOCE INTER + $COVARIANCE
#   3. re-simulate the parameters from an NWPRI prior built out of that fit
#   4. store estimates, covariance matrix and the NWPRI draws in the fixture
#
# Note on step 3: NWPRI is the reference, so the priors have to be centred on
# the fit itself. THETAs get a multivariate-normal prior with the estimated
# THETA block of the covariance matrix; OMEGA and SIGMA get inverse-Wishart
# priors whose degrees of freedom are chosen so the prior variance matches the
# estimated standard error, df = 2 * (estimate / SE)^2 + 1 (Gisleskog, Karlsson
# and Beal 2002), which is the same rule PsN uses.

library(cli)
library(reticulate)

NONMEM  <- "/opt/NONMEM/nm_current/run/nmfe76"
N_DRAWS <- 1000L
SEED    <- 20260820L

stopifnot(file.exists(NONMEM))

fixtures <- normalizePath(".")
work     <- file.path(tempdir(), "nwpri")
dir.create(work, recursive = TRUE, showWarnings = FALSE)

run_nonmem <- function(mod, lst) {
  status <- system2(NONMEM, c(mod, lst), stdout = FALSE, stderr = FALSE)
  if (status != 0) cli::cli_abort("NONMEM failed on {mod} (exit {status}).")
}

# 1. dataset ------------------------------------------------------------------
# 60 subjects, single 100 unit oral dose, 10 samples each; 30% CV IIV on CL and
# V, 20% proportional residual error. Simulated analytically so the fixture does
# not depend on a simulation tool.
set.seed(SEED)
n_id  <- 60
dose  <- 100
tobs  <- c(0.25, 0.5, 1, 2, 4, 6, 8, 12, 16, 24)
ka    <- 1.0
cl    <- 5.0
vc    <- 50.0

dat <- do.call(rbind, lapply(seq_len(n_id), function(i) {
  cl_i <- cl * exp(rnorm(1, 0, sqrt(0.09)))
  v_i  <- vc * exp(rnorm(1, 0, sqrt(0.09)))
  ke   <- cl_i / v_i
  ipred <- dose / v_i * ka / (ka - ke) * (exp(-ke * tobs) - exp(-ka * tobs))
  dv    <- ipred * (1 + rnorm(length(tobs), 0, 0.2))
  rbind(
    data.frame(ID = i, TIME = 0, DV = 0, AMT = dose, EVID = 1, MDV = 1, CMT = 1),
    data.frame(ID = i, TIME = tobs, DV = round(pmax(dv, 1e-4), 4), AMT = 0,
               EVID = 0, MDV = 0, CMT = 2)
  )
}))
write.csv(dat, file.path(work, "data.csv"), row.names = FALSE, quote = FALSE)

# 2. estimation ---------------------------------------------------------------
est_code <- '$PROBLEM NWPRI anchor model

$INPUT ID TIME DV AMT EVID MDV CMT
$DATA data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
KA = THETA(1)
CL = THETA(2)*EXP(ETA(1))
V  = THETA(3)*EXP(ETA(2))
S2 = V

$ERROR
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
writeLines(est_code, file.path(work, "est.mod"))

owd <- setwd(work)
on.exit(setwd(owd), add = TRUE)
run_nonmem("est.mod", "est.lst")

lst <- readLines("est.lst")
if (!any(grepl("MINIMIZATION SUCCESSFUL", lst))) {
  cli::cli_abort("Estimation did not minimise successfully.")
}
if (any(grepl("COVARIANCE MATRIX UNOBTAINABLE", lst))) {
  cli::cli_abort("Covariance step failed; the anchor needs a covariance matrix.")
}

# 3. read estimates and covariance --------------------------------------------
ext     <- readLines("est.ext")
ext_hdr <- strsplit(trimws(ext[2]), "[ ]+")[[1]][-1]
ext_row <- function(tag) {
  line <- grep(paste0("^ *", tag, " "), ext, value = TRUE)[1]
  setNames(as.numeric(strsplit(trimws(line), "[ ]+")[[1]][-1]), ext_hdr)
}
estimates <- ext_row("-1000000000")
std_err   <- ext_row("-1000000001")
estimates <- estimates[names(estimates) != "OBJ"]
std_err   <- std_err[names(std_err) != "OBJ"]

cov_lines <- readLines("est.cov")
cov_names <- strsplit(trimws(cov_lines[2]), "[ ]+")[[1]][-1]
cov_mat   <- do.call(rbind, lapply(
  strsplit(trimws(cov_lines[-(1:2)]), "[ ]+"),
  function(x) as.numeric(x[-1])
))
dimnames(cov_mat) <- list(cov_names, cov_names)

# OMEGA(2,1) is a structurally-fixed off-diagonal; NONMEM reports it as an
# all-zero row/column, which is not positive definite and is not a parameter as
# far as pharmpy is concerned.
keep      <- setdiff(cov_names, "OMEGA(2,1)")
cov_mat   <- cov_mat[keep, keep, drop = FALSE]
estimates <- estimates[keep]
std_err   <- std_err[keep]

# NONMEM names -> pharmpy names (taken from the $THETA/$OMEGA/$SIGMA comments)
pharmpy_names <- c(
  "THETA1"     = "POP_KA",
  "THETA2"     = "POP_CL",
  "THETA3"     = "POP_V",
  "OMEGA(1,1)" = "IIV_CL",
  "OMEGA(2,2)" = "IIV_V",
  "SIGMA(1,1)" = "sigma_prop"
)
rename <- function(x) unname(pharmpy_names[x])
names(estimates) <- rename(names(estimates))
names(std_err)   <- rename(names(std_err))
dimnames(cov_mat) <- list(rename(keep), rename(keep))

# 4. NWPRI reference simulation -----------------------------------------------
theta_names <- c("POP_KA", "POP_CL", "POP_V")
theta_cov   <- cov_mat[theta_names, theta_names, drop = FALSE]

# inverse-Wishart degrees of freedom matching the estimated SE
iw_df <- function(estimate, se) 2 * (estimate / se)^2 + 1
om_df <- iw_df(estimates[c("IIV_CL", "IIV_V")], std_err[c("IIV_CL", "IIV_V")])
sg_df <- iw_df(estimates[["sigma_prop"]], std_err[["sigma_prop"]])

fmt <- function(x) formatC(x, format = "e", digits = 6)
theta_pv <- paste(
  vapply(seq_len(3), function(i) paste(fmt(theta_cov[i, seq_len(i)]), collapse = " "),
         character(1)),
  collapse = "\n"
)

prior_code <- sprintf('$PROBLEM NWPRI reference: sample parameters from the fit

$INPUT ID TIME DV AMT EVID MDV CMT
$DATA prior_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

; PLEV is the probability mass THETA draws are truncated to. It has no default
; that works for TRUE=PRIOR, and is set close to 1 so truncation is negligible
; and the draws stay comparable to an untruncated multivariate normal.
$PRIOR NWPRI PLEV=0.9999

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

$THETA (0, %s) ; POP_KA
$THETA (0, %s) ; POP_CL
$THETA (0, %s) ; POP_V
$OMEGA %s ; IIV_CL
$OMEGA %s ; IIV_V
$SIGMA %s ; sigma_prop

$THETAP (%s FIX) (%s FIX) (%s FIX)
$THETAPV BLOCK(3) FIX
%s

$OMEGAP %s FIX
$OMEGAP %s FIX
$OMEGAPD (%s FIX) (%s FIX)

$SIGMAP %s FIX
$SIGMAPD (%s FIX)

$SIMULATION (%d) SUBPROBLEMS=%d TRUE=PRIOR ONLYSIM

$TABLE ID T1 T2 T3 O11 O22 S11 FIRSTONLY NOAPPEND NOPRINT ONEHEADER FILE=priortab
',
  fmt(estimates[["POP_KA"]]), fmt(estimates[["POP_CL"]]), fmt(estimates[["POP_V"]]),
  fmt(estimates[["IIV_CL"]]), fmt(estimates[["IIV_V"]]), fmt(estimates[["sigma_prop"]]),
  fmt(estimates[["POP_KA"]]), fmt(estimates[["POP_CL"]]), fmt(estimates[["POP_V"]]),
  theta_pv,
  fmt(estimates[["IIV_CL"]]), fmt(estimates[["IIV_V"]]),
  fmt(om_df[["IIV_CL"]]), fmt(om_df[["IIV_V"]]),
  fmt(estimates[["sigma_prop"]]), fmt(sg_df),
  SEED, N_DRAWS
)
writeLines(prior_code, "prior.mod")

# The parameters are drawn once per subproblem and tabled with FIRSTONLY, so a
# minimal single-subject dataset is enough here.
writeLines(c("ID,TIME,DV,AMT,EVID,MDV,CMT",
             "1,0,0,100,1,1,1",
             "1,1,0,0,0,0,2"), "prior_data.csv")

run_nonmem("prior.mod", "prior.lst")

tab      <- readLines("priortab")
tab_hdr  <- strsplit(trimws(tab[2]), "[ ]+")[[1]]
tab_body <- tab[!grepl("TABLE NO|^ ID ", tab)]
draws    <- as.data.frame(do.call(rbind, lapply(
  strsplit(trimws(tab_body), "[ ]+"), as.numeric
)))
names(draws) <- tab_hdr
draws <- draws[, c("T1", "T2", "T3", "O11", "O22", "S11")]
names(draws) <- c("POP_KA", "POP_CL", "POP_V", "IIV_CL", "IIV_V", "sigma_prop")
stopifnot(nrow(draws) == N_DRAWS)

# 5. write the fixture --------------------------------------------------------
anchor <- list(
  model_code          = est_code,
  parameter_estimates = estimates,
  standard_errors     = std_err,
  covariance_matrix   = cov_mat,
  nwpri_draws         = draws,
  meta = list(
    nonmem            = "7.6.0",
    n_draws           = N_DRAWS,
    seed              = SEED,
    plev              = 0.9999,
    omega_prior_df    = om_df,
    sigma_prior_df    = sg_df,
    created           = Sys.Date()
  )
)
saveRDS(anchor, file.path(fixtures, "nwpri_anchor.rds"), version = 2)
cli::cli_alert_success("Wrote nwpri_anchor.rds ({N_DRAWS} NWPRI draws).")
