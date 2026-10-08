# AUC_SS: per-subject dose and bioavailability --------------------------------

.oral_nlmixr_code <- function(f_line = "    f(A_DEPOT) <- F_BIO\n") {
  paste0(
    "sim_model <- function() {\n",
    "  ini({\n",
    "    POP_CL <- 5\n",
    "    POP_V <- 50\n",
    "    POP_KA <- 1\n",
    "    POP_F <- 0.6\n",
    "    eta.cl ~ 0.09\n",
    "    eta.f ~ 0.09\n",
    "    add_err <- 0.001\n",
    "  })\n",
    "  model({\n",
    "    CL <- POP_CL * exp(eta.cl)\n",
    "    V <- POP_V\n",
    "    KA <- POP_KA\n",
    "    F_BIO <- POP_F * exp(eta.f)\n",
    "    d/dt(A_DEPOT) <- -KA * A_DEPOT\n",
    "    d/dt(A_CENTRAL) <- KA * A_DEPOT - CL/V * A_CENTRAL\n",
    f_line,
    "    IPRED <- A_CENTRAL / V\n",
    "    IPRED ~ add(add_err)\n",
    "  })\n",
    "}\n"
  )
}

## Two subjects on different doses, q12h to steady state, observed densely
## over the last interval.
.oral_ss_dat <- function(doses = c(100, 400), n_doses = 20, tau = 12) {
  t_last <- (n_doses - 1) * tau
  obs_t <- seq(t_last, t_last + tau, by = 0.1)
  do.call(rbind, lapply(seq_along(doses), function(i) {
    rbind(
      data.frame(ID = i, TIME = (seq_len(n_doses) - 1) * tau, DV = 0,
                 AMT = doses[i], EVID = 1, MDV = 1, CMT = 1),
      data.frame(ID = i, TIME = obs_t, DV = 0, AMT = 0, EVID = 0, MDV = 0,
                 CMT = 2)
    )
  }))
}

test_that("add_nlmixr_bioavailability_outputs exposes f() as an output", {
  res <- add_nlmixr_bioavailability_outputs(.oral_nlmixr_code())
  expect_equal(res$bioavailability, c("1" = "BIOAV_CMT1", A_DEPOT = "BIOAV_CMT1"))
  lines <- strsplit(res$code, "\n")[[1]]
  i <- grep("f(A_DEPOT)", lines, fixed = TRUE)
  expect_equal(trimws(lines[i - 1]), "BIOAV_CMT1 <- F_BIO")
  ## compartment numbering follows the states: dosing into the central
  ## compartment is compartment 2
  res <- add_nlmixr_bioavailability_outputs(
    .oral_nlmixr_code("    f(A_CENTRAL) = 0.5;\n")
  )
  expect_equal(res$bioavailability,
               c("2" = "BIOAV_CMT2", A_CENTRAL = "BIOAV_CMT2"))
  expect_match(res$code, "BIOAV_CMT2 <- 0.5\n", fixed = TRUE)
})

test_that("add_nlmixr_bioavailability_outputs leaves a model without f() alone", {
  code <- .oral_nlmixr_code("")
  res <- add_nlmixr_bioavailability_outputs(code)
  expect_identical(res$code, code)
  expect_null(res$bioavailability)
})

test_that("run_sim_nlmixr: AUC_SS is each subject's F * dose / CL", {
  skip_if_not_installed("rxode2")
  dat <- .oral_ss_dat()
  out <- run_sim_nlmixr(
    data = dat, model_code = .oral_nlmixr_code(), seed = 3,
    add_pk_variables = TRUE, verbose = FALSE
  )
  ## the helper column is not part of the output
  expect_false("BIOAV_CMT1" %in% names(out))
  out <- out[out$EVID == 0, ]
  for(id in 1:2) {
    sub <- out[out$ID == id, ]
    dose <- unique(dat$AMT[dat$ID == id & dat$EVID == 1])
    expect_equal(unique(sub$AUC_SS), unique(sub$F_BIO * dose / sub$CL))
    ## and it is the AUC over a steady-state interval, as the simulation has it
    auc_trap <- sum(diff(sub$TIME) *
                      (utils::head(sub$IPRED, -1) + utils::tail(sub$IPRED, -1)) / 2)
    expect_equal(unique(sub$AUC_SS), auc_trap, tolerance = 0.01)
  }
})

test_that("the NONMEM simulation table carries the model's bioavailability", {
  skip_if_nonmem_not_available()
  mod <- suppressMessages(create_model(route = "oral", bioavailability = TRUE))
  sim_model <- build_nonmem_sim_model(mod, seed = 1, n_iterations = 1,
                                      verbose = FALSE)
  table_code <- strsplit(sim_model$code, "$TABLE", fixed = TRUE)[[1]][2]
  expect_match(table_code, "\\bF1\\b")
  expect_match(table_code, "\\bCL\\b")
})
