# $PRIOR NWPRI record construction (#130) -------------------------------------
#
# The point of these tests is that the *model file* comes out right. The
# reference is `fixtures/nwpri_generated_anchor.rds`, which holds, for three
# models chosen to exercise the three ways the records differ:
#
#   * the model code the prior records were inserted into (`input_code`),
#   * the parameter structure pharmpy reported for it (`param_structure`),
#   * the control stream `add_nwpri_prior()` produced (`generated_code`),
#   * and 1000 parameter vectors NONMEM 7.6.0 drew from that control stream.
#
# So the frozen `generated_code` is not just "what the function used to emit";
# it is a stream NONMEM has actually run. Reproducing it byte for byte from
# `build_nwpri_records()` + `insert_nwpri_records()` therefore checks the record
# construction against NONMEM rather than against itself, and does so without
# needing pharmpy or NONMEM at test time.
#
# See `fixtures/_create-nwpri-generated-anchor.R` for how the fixture is built.

.nwpri_generated <- function() {
  readRDS(test_path("fixtures", "nwpri_generated_anchor.rds"))
}

.nwpri_case <- function(name) .nwpri_generated()$cases[[name]]

.nwpri_case_names <- c("diagonal", "block", "fixed")

## Rebuild a case's control stream from the frozen inputs, without pharmpy.
.nwpri_rebuild <- function(case) {
  records <- build_nwpri_records(
    param_structure     = case$param_structure,
    parameter_estimates = case$parameter_estimates,
    covariance_matrix   = case$covariance_matrix,
    plev                = 0.9999
  )
  insert_nwpri_records(case$input_code, records)
}

## Records of a given type, as they appear in a control stream.
.records_of <- function(code, name) {
  lines <- unlist(strsplit(code, "\n"))
  starts <- grep("^\\$", lines)
  bounds <- c(starts, length(lines) + 1L)
  hit <- grepl(paste0("^\\$", name, "\\b"), lines[starts])
  lapply(which(hit), function(k) lines[seq.int(bounds[k], bounds[k + 1L] - 1L)])
}

.numbers_in <- function(x) {
  as.numeric(unlist(regmatches(
    x, gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", x)
  )))
}

# Record construction, anchored to the NONMEM-accepted streams ----------------

test_that("the emitted control stream reproduces the NONMEM-run reference", {
  for (name in .nwpri_case_names) {
    case <- .nwpri_case(name)
    ## The `fixed` case has a parameter the covariance matrix does not cover,
    ## which is warned about by design.
    rebuilt <- suppressWarnings(.nwpri_rebuild(case))
    expect_identical(rebuilt, case$generated_code, info = name)
  }
})

test_that("the prior records are the only thing added to the model", {
  for (name in .nwpri_case_names) {
    case <- .nwpri_case(name)
    added <- setdiff(
      unlist(strsplit(case$generated_code, "\n")),
      unlist(strsplit(case$input_code, "\n"))
    )
    added <- added[nzchar(trimws(added))]
    ## Every added line either opens a prior record or continues one (the
    ## BLOCK() lower triangles). Nothing else about the model may change.
    expect_true(
      all(grepl("^\\$(PRIOR|THETAP|THETAPV|OMEGAP|OMEGAPD|SIGMAP|SIGMAPD)\\b", added) |
            grepl("^[-+0-9. e]+$", added)),
      info = name
    )
    ## and no line of the original is lost
    expect_length(
      setdiff(unlist(strsplit(case$input_code, "\n")),
              unlist(strsplit(case$generated_code, "\n"))),
      0
    )
  }
})

test_that("$PRIOR carries PLEV and sits before the model's own records", {
  case <- .nwpri_case("diagonal")
  code <- .nwpri_rebuild(case)
  lines <- unlist(strsplit(code, "\n"))

  prior_at <- grep("^\\$PRIOR", lines)
  expect_length(prior_at, 1)
  expect_match(lines[prior_at], "^\\$PRIOR NWPRI PLEV=0.9999$")
  ## NM-TRAN wants $PRIOR before $THETA; NONMEM has no usable PLEV default for
  ## TRUE=PRIOR, so it must always be written out.
  expect_lt(prior_at, min(grep("^\\$THETA\\b", lines)))
  expect_lt(prior_at, min(grep("^\\$PK\\b", lines)))
})

test_that("$THETAP holds the point estimates and $THETAPV the THETA covariance", {
  case <- .nwpri_case("diagonal")
  code <- .nwpri_rebuild(case)

  thetas <- case$param_structure$theta
  thetap <- .records_of(code, "THETAP")[[1]]
  expect_equal(.numbers_in(thetap), unname(case$parameter_estimates[thetas]),
               tolerance = 1e-6)

  thetapv <- .records_of(code, "THETAPV")[[1]]
  expect_match(thetapv[1], paste0("^\\$THETAPV BLOCK\\(", length(thetas), "\\) FIX$"))
  expected <- case$covariance_matrix[thetas, thetas, drop = FALSE]
  expect_equal(
    .numbers_in(thetapv[-1]),
    ## lower triangle, row by row
    unlist(lapply(seq_along(thetas), function(i) expected[i, seq_len(i)])),
    tolerance = 1e-6, ignore_attr = TRUE
  )
})

test_that("$OMEGAP mirrors the model's own $OMEGA block structure", {
  diag_code  <- .nwpri_rebuild(.nwpri_case("diagonal"))
  block_code <- .nwpri_rebuild(.nwpri_case("block"))

  ## Two 1x1 blocks -> two scalar records, one df each.
  omegap <- .records_of(diag_code, "OMEGAP")
  expect_length(omegap, 2)
  expect_true(all(vapply(omegap, function(r) grepl("FIX$", r[1]), logical(1))))
  expect_false(any(grepl("BLOCK", unlist(omegap))))
  expect_length(.numbers_in(.records_of(diag_code, "OMEGAPD")[[1]]), 2)

  ## One 2x2 block -> one BLOCK(2) record with a lower triangle, and a single
  ## df for the whole block (not one per element).
  omegap <- .records_of(block_code, "OMEGAP")
  expect_length(omegap, 1)
  expect_match(omegap[[1]][1], "^\\$OMEGAP BLOCK\\(2\\) FIX$")
  est <- .nwpri_case("block")$parameter_estimates
  expect_equal(
    .numbers_in(omegap[[1]][-1]),
    unname(c(est[["IIV_CL"]], est[["OMEGA_2_1"]], est[["IIV_V"]])),
    tolerance = 1e-6
  )
  expect_length(.numbers_in(.records_of(block_code, "OMEGAPD")[[1]]), 1)

  ## Two separate $SIGMA records stay two separate $SIGMAP records.
  expect_length(.records_of(block_code, "SIGMAP"), 2)
  expect_length(.numbers_in(.records_of(block_code, "SIGMAPD")[[1]]), 2)
})

test_that("the degrees of freedom follow the estimate/SE rule", {
  case <- .nwpri_case("diagonal")
  code <- .nwpri_rebuild(case)
  est  <- case$parameter_estimates
  se   <- sqrt(diag(case$covariance_matrix))

  expected <- vapply(
    c("IIV_CL", "IIV_V"),
    function(p) 2 * (est[[p]] / se[[p]])^2 + 1,
    numeric(1)
  )
  expect_equal(.numbers_in(.records_of(code, "OMEGAPD")[[1]]),
               unname(expected), tolerance = 1e-5)
  expect_equal(
    .numbers_in(.records_of(code, "SIGMAPD")[[1]]),
    2 * (est[["sigma_prop"]] / se[["sigma_prop"]])^2 + 1,
    tolerance = 1e-5
  )
})

test_that("nwpri_block_df reduces to the Gisleskog rule for a 1x1 block", {
  expect_equal(nwpri_block_df(0.09, 0.01), 2 * (0.09 / 0.01)^2 + 1)
  ## for a block it averages over the diagonal and adds the block dimension
  expect_equal(
    nwpri_block_df(c(0.09, 0.04), c(0.01, 0.01)),
    mean(c(2 * 81, 2 * 16)) + 2
  )
  ## a more precisely estimated parameter gets more degrees of freedom
  expect_gt(nwpri_block_df(0.09, 0.001), nwpri_block_df(0.09, 0.01))
})

# Parameters the covariance matrix does not cover ------------------------------

test_that("uncovered parameters are warned about and given a negligible variance", {
  case <- .nwpri_case("fixed")

  expect_warning(code <- .nwpri_rebuild(case), "POP_KA")
  thetapv <- .records_of(code, "THETAPV")[[1]]
  ## POP_KA is THETA(1), so its variance is the first element of the triangle.
  var_ka <- .numbers_in(thetapv[-1])[1]
  est_ka <- case$parameter_estimates[["POP_KA"]]
  expect_equal(sqrt(var_ka) / est_ka, 1e-3, tolerance = 1e-9)
  ## still present in $THETAP at its point estimate, since $THETAP is positional
  expect_equal(.numbers_in(.records_of(code, "THETAP")[[1]])[1], est_ka,
               tolerance = 1e-6)
})

test_that("zero covariances inside a BLOCK record are nudged off zero", {
  ## NM-TRAN: `64 A COVARIANCE IS ZERO, BUT THE BLOCK IS NOT A BAND MATRIX`.
  m <- matrix(c(4, 0, 0, 0, 9, 0, 0, 0, 16), 3, 3)
  filled <- nwpri_fill_zero_covariances(m)
  expect_equal(diag(filled), diag(m))
  expect_true(all(filled[lower.tri(filled)] > 0))
  expect_equal(filled[2, 1], 1e-6 * sqrt(4 * 9))
  expect_equal(filled, t(filled))
  ## non-zero covariances are left alone
  m2 <- matrix(c(4, 1, 1, 9), 2, 2)
  expect_equal(nwpri_fill_zero_covariances(m2), m2)
  ## a 1x1 block has no off-diagonal to worry about
  expect_equal(nwpri_fill_zero_covariances(matrix(4, 1, 1)), matrix(4, 1, 1))
})

test_that("no zero covariance survives into a BLOCK record", {
  for (name in .nwpri_case_names) {
    code <- suppressWarnings(.nwpri_rebuild(.nwpri_case(name)))
    blocks <- c(.records_of(code, "THETAPV"), .records_of(code, "OMEGAP"),
                .records_of(code, "SIGMAP"))
    for (b in blocks) {
      if (!grepl("BLOCK", b[1])) next
      expect_false(any(.numbers_in(b[-1]) == 0), info = paste(name, b[1]))
    }
  }
})

# Validation -------------------------------------------------------------------

test_that("build_nwpri_records validates plev", {
  case <- .nwpri_case("diagonal")
  build <- function(plev) {
    build_nwpri_records(case$param_structure, case$parameter_estimates,
                        case$covariance_matrix, plev = plev)
  }
  expect_error(build(0), "between 0 and 1")
  expect_error(build(1), "between 0 and 1")
  expect_error(build(-0.5), "between 0 and 1")
  expect_error(build(c(0.9, 0.99)), "between 0 and 1")
  expect_error(build("0.9"), "between 0 and 1")
  expect_match(build(0.95)$prior, "PLEV=0.95")
})

test_that("build_nwpri_records rejects unusable estimates and covariances", {
  case <- .nwpri_case("diagonal")
  est  <- case$parameter_estimates
  cov  <- case$covariance_matrix

  expect_error(
    build_nwpri_records(case$param_structure, unname(est), cov),
    "named vector"
  )
  expect_error(
    build_nwpri_records(case$param_structure, est[-1], cov),
    "missing model parameters"
  )
  expect_error(
    build_nwpri_records(case$param_structure, est, unname(as.matrix(cov))),
    "parameter names as row/column names"
  )
  scrambled <- cov
  rownames(scrambled) <- paste0("x", rownames(scrambled))
  expect_error(
    build_nwpri_records(case$param_structure, est, scrambled),
    "same parameters"
  )
})

test_that("a covariance matrix whose rows are permuted is realigned, not mislabelled", {
  case <- .nwpri_case("diagonal")
  reordered <- case$covariance_matrix[rev(rownames(case$covariance_matrix)), , drop = FALSE]
  expect_identical(
    build_nwpri_records(case$param_structure, case$parameter_estimates, reordered),
    build_nwpri_records(case$param_structure, case$parameter_estimates,
                        case$covariance_matrix)
  )
})

test_that("build_nwpri_records refuses OMEGA blocks that share parameters", {
  case <- .nwpri_case("block")
  ## What `$OMEGA BLOCK(2) SAME` (IOV) looks like once pharmpy has read it: two
  ## distributions over the same parameters. NWPRI cannot express the SAME
  ## constraint, so each occurrence would be sampled independently.
  iov <- case$param_structure
  iov$omega <- c(iov$omega, iov$omega)
  expect_error(
    build_nwpri_records(iov, case$parameter_estimates, case$covariance_matrix),
    "share parameters"
  )
})

test_that("build_nwpri_records refuses a variance estimated at zero", {
  case <- .nwpri_case("diagonal")
  est <- case$parameter_estimates
  est[["IIV_V"]] <- 0
  expect_error(
    build_nwpri_records(case$param_structure, est, case$covariance_matrix),
    "variance to be positive"
  )
  est[["IIV_V"]] <- -1
  expect_error(
    build_nwpri_records(case$param_structure, est, case$covariance_matrix),
    "variance to be positive"
  )
})

test_that("a zero on the covariance diagonal counts as no uncertainty", {
  case <- .nwpri_case("fixed")
  ## Same model as the `fixed` case, but with POP_KA present in the covariance
  ## matrix as an all-zero row/column, which is how a parameter that was
  ## reported but not estimated tends to arrive.
  cov <- case$covariance_matrix
  padded <- matrix(0, nrow(cov) + 1L, ncol(cov) + 1L,
                   dimnames = list(c("POP_KA", rownames(cov)),
                                   c("POP_KA", colnames(cov))))
  padded[rownames(cov), colnames(cov)] <- cov

  expect_warning(
    with_zero <- build_nwpri_records(case$param_structure,
                                     case$parameter_estimates, padded),
    "POP_KA"
  )
  expect_warning(
    without <- build_nwpri_records(case$param_structure,
                                   case$parameter_estimates,
                                   case$covariance_matrix),
    "POP_KA"
  )
  expect_identical(with_zero, without)
})

test_that("a structurally zero covariance in a BLOCK is a number, not a parameter", {
  ## `random_variables$get_covariance()` reports "0" rather than a parameter
  ## name for a covariance the model fixes to zero, so the block matrices hold
  ## numbers among the names.
  case <- .nwpri_case("block")
  ps <- case$param_structure
  ps$omega[[1]][1, 2] <- ps$omega[[1]][2, 1] <- "0"
  ## ... and the same in a second block, to show that two blocks both reporting
  ## "0" is not two blocks sharing a parameter.
  ps$sigma <- list(matrix(c("sigma_prop", "0", "0", "sigma_add"), nrow = 2))
  est <- case$parameter_estimates[setdiff(names(case$parameter_estimates),
                                          c("OMEGA_2_1", "SIGMA_2_1"))]
  cov <- case$covariance_matrix
  keep <- setdiff(colnames(as.matrix(cov)), "OMEGA_2_1")
  cov <- as.matrix(cov)[keep, keep]

  records <- build_nwpri_records(ps, est, cov)
  omegap <- .records_of(paste(records$records, collapse = "\n"), "OMEGAP")[[1]]
  expect_match(omegap[1], "BLOCK\\(2\\)")
  ## The zero was nudged off zero for NM-TRAN, not written out as `NA`.
  expect_false(any(grepl("NA", records$records)))
  off_diag <- .numbers_in(omegap[3])[1]
  expect_true(off_diag > 0 && off_diag < 1e-4)
})

test_that("build_nwpri_records refuses a variance fixed to a constant", {
  case <- .nwpri_case("block")
  ps <- case$param_structure
  ps$omega[[1]] <- matrix(c("0", "0", "0", "0"), nrow = 2)
  expect_error(
    build_nwpri_records(ps, case$parameter_estimates, case$covariance_matrix),
    "variance to be an estimated parameter"
  )
})

test_that("non-finite covariance elements are warned about and zeroed", {
  ## NONMEM reports NaN for parameters the covariance step could not separate.
  case <- .nwpri_case("diagonal")
  cov <- as.matrix(case$covariance_matrix)
  cov["POP_CL", "POP_V"] <- cov["POP_V", "POP_CL"] <- NaN

  expect_warning(
    records <- build_nwpri_records(case$param_structure,
                                   case$parameter_estimates, cov),
    "non-finite"
  )
  expect_false(any(grepl("NA|NaN", records$records)))

  ## A NaN *variance* means the parameter has no usable uncertainty at all, so
  ## it falls through to the held-fixed path rather than reaching the records.
  cov2 <- as.matrix(case$covariance_matrix)
  cov2["POP_CL", ] <- cov2[, "POP_CL"] <- NaN
  expect_warning(
    expect_warning(
      records2 <- build_nwpri_records(case$param_structure,
                                      case$parameter_estimates, cov2),
      "non-finite"
    ),
    "POP_CL"
  )
  expect_false(any(grepl("NA|NaN", records2$records)))
})

test_that("nwpri_fill_zero_covariances treats a missing covariance as zero", {
  m <- matrix(c(4, NA, NA, 9), nrow = 2)
  filled <- nwpri_fill_zero_covariances(m)
  expect_false(anyNA(filled))
  expect_equal(filled[1, 2], filled[2, 1])
  expect_true(filled[1, 2] > 0 && filled[1, 2] < 1e-4)
})

test_that("insert_nwpri_records refuses a model that already has a $PRIOR", {
  case <- .nwpri_case("diagonal")
  records <- build_nwpri_records(case$param_structure, case$parameter_estimates,
                                 case$covariance_matrix)
  expect_error(
    insert_nwpri_records(case$generated_code, records),
    "already has a .*\\$PRIOR"
  )
})

test_that("insert_nwpri_records needs parameter records to place the prior after", {
  records <- build_nwpri_records(
    .nwpri_case("diagonal")$param_structure,
    .nwpri_case("diagonal")$parameter_estimates,
    .nwpri_case("diagonal")$covariance_matrix
  )
  expect_error(
    insert_nwpri_records("$PROBLEM x\n$INPUT ID TIME DV\n$DATA d.csv\n", records),
    "no \\$THETA/\\$OMEGA/\\$SIGMA records"
  )
  expect_error(insert_nwpri_records("not a control stream", records),
               "No NONMEM records")
})

test_that("the prior parameter records land after the last $THETA/$OMEGA/$SIGMA", {
  case <- .nwpri_case("diagonal")
  lines <- unlist(strsplit(.nwpri_rebuild(case), "\n"))
  last_param <- max(grep("^\\$(THETA|OMEGA|SIGMA)\\b", lines))
  first_prior_param <- min(grep("^\\$(THETAP|OMEGAP|SIGMAP)", lines))
  expect_gt(first_prior_param, last_param)
  ## and before whatever came next in the model
  expect_lt(first_prior_param, min(grep("^\\$ESTIMATION", lines)))
})

test_that("has_prior_record detects a $PRIOR record in code", {
  case <- .nwpri_case("diagonal")
  expect_false(has_prior_record(case$input_code))
  expect_true(has_prior_record(case$generated_code))
})

# The pharmpy side -------------------------------------------------------------

test_that("nwpri_model_structure reads back the model's parameter structure", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  for (name in .nwpri_case_names) {
    case <- .nwpri_case(name)
    model <- pharmr::read_model_from_string(case$input_code)
    expect_equal(nwpri_model_structure(model), case$param_structure, info = name)
  }
})

test_that("add_nwpri_prior emits the NONMEM-run control stream", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  for (name in .nwpri_case_names) {
    case <- .nwpri_case(name)
    model <- pharmr::read_model_from_string(case$input_code)
    prior_model <- suppressWarnings(add_nwpri_prior(
      model,
      parameter_estimates = case$parameter_estimates,
      covariance_matrix   = case$covariance_matrix
    ))
    expect_identical(prior_model$code, case$generated_code, info = name)
  }
})

test_that("add_nwpri_prior requires a fit with a covariance matrix", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  model <- pharmr::read_model_from_string(.nwpri_case("diagonal")$input_code)
  expect_error(add_nwpri_prior(model, fit = NULL), "covariance matrix")
  expect_error(add_nwpri_prior(model, fit = list(parameter_estimates = c(a = 1))),
               "covariance matrix")
  expect_error(add_nwpri_prior("not a model"), "Pharmpy model object")
})

test_that("add_nwpri_prior refuses to add a second $PRIOR", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  case <- .nwpri_case("diagonal")
  model <- pharmr::read_model_from_string(case$input_code)
  prior_model <- add_nwpri_prior(
    model,
    parameter_estimates = case$parameter_estimates,
    covariance_matrix   = case$covariance_matrix
  )
  expect_error(
    add_nwpri_prior(prior_model,
                    parameter_estimates = case$parameter_estimates,
                    covariance_matrix   = case$covariance_matrix),
    "already has"
  )
})

test_that("the generated control stream passes NM-TRAN", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  nmfe <- skip_if_nmfe_not_available()

  case <- .nwpri_case("diagonal")
  folder <- withr::local_tempdir()
  writeLines(c("ID,TIME,DV,AMT,EVID,MDV,CMT",
               "1,0,0,100,1,1,1",
               "1,1,0,0,0,0,2"),
             file.path(folder, "prior_data.csv"))
  model <- pharmr::read_model_from_string(case$input_code)
  prior_model <- add_nwpri_prior(
    model,
    parameter_estimates = case$parameter_estimates,
    covariance_matrix   = case$covariance_matrix
  )
  code <- set_simulation_clean(prior_model, seed = 1, n = 3, true_prior = TRUE)
  code <- paste0(code, "\n\n$TABLE ID TIME NOAPPEND NOPRINT ONEHEADER FILE=simtab\n")
  writeLines(change_nonmem_dataset(code, "prior_data.csv"),
             file.path(folder, "run.mod"))

  ok <- call_nmfe(model_file = "run.mod", output_file = "run.lst",
                  path = folder, nmfe = nmfe, check_only = TRUE)
  expect_true(as.logical(ok))
})

# What NONMEM actually drew from these streams ---------------------------------

test_that("the generated stream draws what the hand-written anchor drew", {
  ours   <- .nwpri_case("diagonal")$draws
  anchor <- readRDS(test_path("fixtures", "nwpri_anchor.rds"))$nwpri_draws

  ## Same fit, same PLEV, same 1000 draws, different NONMEM seed: the two sets
  ## of draws are not identical, but they are draws from the same distribution.
  ## Monte Carlo error on a mean is ~0.1% of the SD, on an SD ~2.2%.
  for (p in names(anchor)) {
    expect_lt(abs(mean(ours[[p]]) / mean(anchor[[p]]) - 1), 0.01)
    expect_lt(abs(stats::sd(ours[[p]]) / stats::sd(anchor[[p]]) - 1), 0.10)
  }
})

test_that("the draws are centred on the fit and spread by its standard errors", {
  for (name in .nwpri_case_names) {
    case <- .nwpri_case(name)
    draws <- case$draws
    est   <- case$parameter_estimates
    se    <- case$standard_errors

    for (p in names(draws)) {
      if (!p %in% names(se)) next
      ## Fixed effects: symmetric prior, so the mean lands on the estimate.
      ## Variance parameters: the inverse-Wishart prior NWPRI uses is
      ## right-skewed, so its mean sits a few percent above the estimate.
      tol_mean <- if (grepl("^POP_", p)) 0.02 else 0.10
      expect_lt(abs(mean(draws[[p]]) / est[[p]] - 1), tol_mean,
                label = paste(name, p, "mean"))
      ## The df rule is chosen so the prior SD matches the estimated SE. The
      ## tolerance is wide for the variance parameters because a low-df
      ## inverse-Wishart is genuinely heavy-tailed, not because of noise.
      tol_sd <- if (grepl("^POP_", p)) 0.10 else 0.30
      expect_lt(abs(stats::sd(draws[[p]]) / se[[p]] - 1), tol_sd,
                label = paste(name, p, "sd"))
    }
  }
})

test_that("a BLOCK(2) OMEGA prior keeps the estimated correlation", {
  case <- .nwpri_case("block")
  draws <- case$draws
  est <- case$parameter_estimates

  expected <- est[["OMEGA_2_1"]] / sqrt(est[["IIV_CL"]] * est[["IIV_V"]])
  expect_gt(expected, 0.3)
  drawn <- mean(draws$OMEGA_2_1 / sqrt(draws$IIV_CL * draws$IIV_V))
  expect_lt(abs(drawn - expected), 0.05)
  ## the off-diagonal genuinely varies, i.e. the block is being sampled
  expect_gt(stats::sd(draws$OMEGA_2_1), 0)
})

test_that("a parameter outside the covariance matrix comes back unchanged", {
  case <- .nwpri_case("fixed")
  draws <- case$draws
  expect_equal(stats::sd(draws$POP_KA), 0)
  expect_equal(unique(draws$POP_KA), case$parameter_estimates[["POP_KA"]],
               tolerance = 1e-5, ignore_attr = TRUE)
  ## and the parameters that do have uncertainty are unaffected by it
  expect_gt(stats::sd(draws$POP_CL), 0)
  expect_gt(stats::sd(draws$IIV_CL), 0)
})
