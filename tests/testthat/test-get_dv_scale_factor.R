# ── parse_dv_scale() ────────────────────────────────────────────────────────

test_that("parse_dv_scale reads a bare variable as factor 1", {
  expect_equal(parse_dv_scale("V2"), list(variable = "V2", factor = 1))
})

test_that("parse_dv_scale reads division as the AUC multiplier", {
  ## S2 = V2/1000 -> concentrations, and so AUC, are 1000x the plain
  ## amount/volume units
  expect_equal(parse_dv_scale("V2/1000"), list(variable = "V2", factor = 1000))
  expect_equal(parse_dv_scale("V2 / 1000"), list(variable = "V2", factor = 1000))
  expect_equal(parse_dv_scale("V2/1e3"), list(variable = "V2", factor = 1000))
  expect_equal(parse_dv_scale("V2/0.5"), list(variable = "V2", factor = 0.5))
})

test_that("parse_dv_scale reads multiplication, either way round", {
  expect_equal(parse_dv_scale("V2*1000"), list(variable = "V2", factor = 1e-3))
  expect_equal(parse_dv_scale("1000*V2"), list(variable = "V2", factor = 1e-3))
})

test_that("parse_dv_scale returns NULL for anything it cannot read", {
  expect_null(parse_dv_scale("V2/WT"))
  expect_null(parse_dv_scale("V2+1"))
  expect_null(parse_dv_scale("V2/(1000*WT)"))
  expect_null(parse_dv_scale("V2/0"))
})

# ── get_dv_scale_factor() ───────────────────────────────────────────────────

## A stand-in for a Pharmpy model: only `$statements$find_assignment()` is
## reached, the rest of the model is mocked away below.
.fake_model <- function(assignments = list()) {
  list(statements = list(
    find_assignment = function(name) {
      if(is.null(assignments[[name]])) return(NULL)
      list(expression = assignments[[name]])
    }
  ))
}

.local_nonmem_model <- function(compartment = 2, volume = "V2", env = parent.frame()) {
  local_mocked_bindings(
    get_tool_from_model = function(...) "nonmem",
    get_obs_compartment = function(...) compartment,
    find_pk_parameter = function(...) volume,
    .env = env
  )
}

test_that("get_dv_scale_factor: 1 for a NULL model", {
  expect_equal(get_dv_scale_factor(NULL), 1)
})

test_that("get_dv_scale_factor: 1 for a non-NONMEM model (no S<n> there)", {
  local_mocked_bindings(get_tool_from_model = function(...) "nlmixr")
  expect_equal(get_dv_scale_factor(.fake_model(list(S2 = "V2/1000"))), 1)
})

test_that("get_dv_scale_factor: 1 for the usual S<n> = V", {
  .local_nonmem_model()
  expect_equal(get_dv_scale_factor(.fake_model(list(S2 = "V2"))), 1)
})

test_that("get_dv_scale_factor: picks up S2 = V2/1000", {
  .local_nonmem_model()
  expect_equal(get_dv_scale_factor(.fake_model(list(S2 = "V2/1000"))), 1000)
})

test_that("get_dv_scale_factor: picks up S2 = V2*1000", {
  .local_nonmem_model()
  expect_equal(get_dv_scale_factor(.fake_model(list(S2 = "V2*1000"))), 1e-3)
})

test_that("get_dv_scale_factor: 1 when no S<n> is defined", {
  ## NONMEM scales by 1 then, i.e. the model predicts amounts, and no
  ## volume-based correction of dose/CL applies
  .local_nonmem_model()
  expect_equal(get_dv_scale_factor(.fake_model(list(S1 = "V1/1000"))), 1)
})

test_that("get_dv_scale_factor: reads the observation compartment's S", {
  .local_nonmem_model(compartment = 1, volume = "V1")
  model <- .fake_model(list(S1 = "V1/1000", S2 = "V2"))
  expect_equal(get_dv_scale_factor(model), 1000)
  ## explicit `compartment` wins over the inferred one
  expect_equal(get_dv_scale_factor(model, compartment = 2), 1)
})

test_that("get_dv_scale_factor: warns and returns 1 for a scaling it cannot read", {
  .local_nonmem_model()
  expect_warning(
    factor <- get_dv_scale_factor(.fake_model(list(S2 = "WT"))),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
  expect_warning(
    factor <- get_dv_scale_factor(.fake_model(list(S2 = "V2/WT"))),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
})

test_that("get_dv_scale_factor: accepts the common central volume names", {
  for(volume in c("V", "V1", "V2", "VC")) {
    .local_nonmem_model(volume = "SOMETHING_ELSE")
    expect_equal(
      get_dv_scale_factor(.fake_model(stats::setNames(
        list(paste0(volume, "/1000")), "S2"
      ))),
      1000
    )
  }
})

# ── dv_scale_from_code() (nlmixr2 / rxode2 code) ────────────────────────────

test_that("dv_scale_from_code: 1 when the prediction divides by the volume", {
  expect_equal(dv_scale_from_code(c("  IPRED <- A_CENTRAL/VC", "  Y <- IPRED")), 1)
})

test_that("dv_scale_from_code: reads the injected S<n> rewrite", {
  expect_equal(
    dv_scale_from_code(c("  S2 <- VC/1000", "  IPRED <- A_CENTRAL/S2")),
    1000
  )
  expect_equal(
    dv_scale_from_code("F <- A_CENTRAL/S1\nS1 <- V*1000\n"),
    1e-3
  )
})

test_that("dv_scale_from_code: round-trips inject_nlmixr_scaling()", {
  code <- paste(c(
    "mod <- function() {",
    "  model({",
    "    CL <- POP_CL*exp(ETA_CL)",
    "    VC <- POP_VC*exp(ETA_VC)",
    "    d/dt(A_DEPOT) = -KA*A_DEPOT",
    "    d/dt(A_CENTRAL) = KA*A_DEPOT - CL*A_CENTRAL/VC",
    "    IPRED <- A_CENTRAL/VC",
    "    Y <- IPRED",
    "  })",
    "}"
  ), collapse = "\n")
  expect_equal(dv_scale_from_code(code), 1)
  expect_equal(dv_scale_from_code(inject_nlmixr_scaling(code, 1000)), 1000)
})

test_that("dv_scale_from_code: 1 for code with no prediction of that shape", {
  expect_equal(dv_scale_from_code("Y <- IPRED + IPRED*EPS1"), 1)
  ## S<n> divided by, but never defined
  expect_equal(dv_scale_from_code("IPRED <- A_CENTRAL/S2"), 1)
})

test_that("dv_scale_from_code: warns and returns 1 for a scaling it cannot read", {
  expect_warning(
    factor <- dv_scale_from_code(c("S2 <- VC/WT", "IPRED <- A_CENTRAL/S2")),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
})

test_that("dv_scale_from_code: rejects a scaling that is not the central volume", {
  ## `S2 <- WT/1000` parses as cleanly as `VC/1000`, but the factor on dose/CL
  ## is `V/S`, which is not a constant here
  expect_warning(
    factor <- dv_scale_from_code(c("S2 <- WT/1000", "IPRED <- A_CENTRAL/S2")),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
  ## nor is a peripheral volume the central one
  expect_warning(
    factor <- dv_scale_from_code(c(
      "d/dt(A_CENTRAL) = -CL*A_CENTRAL/VC",
      "S2 <- VP/1000",
      "IPRED <- A_CENTRAL/S2"
    )),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
})

test_that("dv_scale_from_code: takes the volume name from the ODE", {
  ## a volume that is not one of the names pharmpy generates is still the
  ## central volume if that is what the central compartment eliminates through
  for(ode in c(
    "d/dt(A_CENTRAL) = KA*A_DEPOT - CL*A_CENTRAL/VCEN",
    "d/dt(A_CENTRAL) = -A_CENTRAL*CL/VCEN",
    "d/dt(A_CENTRAL) = -(CL/VCEN)*A_CENTRAL"
  )) {
    expect_equal(
      dv_scale_from_code(c(ode, "S2 <- VCEN/1000", "IPRED <- A_CENTRAL/S2")),
      1000
    )
  }
})

test_that("dv_scale_from_code: follows the alias pharmpy emits for the volume", {
  ## pharmpy writes `V <- VC` and eliminates through `V`, but scales `VC`
  expect_equal(
    dv_scale_from_code(c(
      "VC <- TVV*exp(ETA_VC)",
      "V <- VC",
      "S1 <- VC/1000",
      "d/dt(A_CENTRAL) = -CL*A_CENTRAL/V",
      "F <- A_CENTRAL/S1"
    )),
    1000
  )
})

test_that("dv_scale_from_code: reads this package's own nlmixr2 template", {
  ## `create_model_nlmixr()` carries the `S2 <- V/1000` scaling in its body
  template <- paste(deparse(create_model_nlmixr), collapse = "\n")
  expect_equal(dv_scale_from_code(template), 1000)
})

test_that("get_dv_scale_factor: `code` is read instead of `model`", {
  expect_equal(
    get_dv_scale_factor(code = c("S2 <- VC/1000", "IPRED <- A_CENTRAL/S2")),
    1000
  )
})
