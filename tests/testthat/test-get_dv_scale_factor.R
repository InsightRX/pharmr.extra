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

## `central_volume_names()` asks pharmpy for the model's topology first, which
## a stand-in model cannot answer, so it falls through to `find_pk_parameter()`
## -- mocked here. `volume = NULL` stands for a model whose central volume
## cannot be read at all.
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
  model <- .fake_model(list(S1 = "V1/1000", S2 = "V1"))
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

test_that("get_dv_scale_factor: rejects a volume that is not the central one", {
  ## `S2 = V3/1000` on a model whose central volume is V2 parses just as
  ## cleanly, but its factor is `1000 * V2/V3` -- not a constant
  .local_nonmem_model(volume = "V2")
  expect_warning(
    factor <- get_dv_scale_factor(.fake_model(list(S2 = "V3/1000"))),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
})

test_that("get_dv_scale_factor: accepts an alias of the central volume", {
  ## pharmpy emits `V = VC` and either name may carry the scaling
  .local_nonmem_model(volume = "VC")
  expect_equal(
    get_dv_scale_factor(.fake_model(list(S2 = "V/1000", V = "VC"))),
    1000
  )
})

test_that("get_dv_scale_factor: falls back to the common volume names", {
  ## only where the model's own central volume cannot be read at all
  for(volume in c("V", "V1", "V2", "VC")) {
    .local_nonmem_model(volume = NULL)
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

test_that("dv_scale_from_code: rejects a peripheral volume in a two-compartment ODE", {
  ## `(-CL/V1 - Q/V1)*A_CENTRAL` puts another term before the amount, so the
  ## volume has to come from the clearance term itself rather than from
  ## whatever sits next to `A_CENTRAL`
  ode <- "d/dt(A_CENTRAL) = (-CL/V1 - Q/V1)*A_CENTRAL + Q/V2*A_PERIPH"
  expect_warning(
    factor <- dv_scale_from_code(c(ode, "S2 <- V2/1000", "IPRED <- A_CENTRAL/S2")),
    "Could not interpret the scaling"
  )
  expect_equal(factor, 1)
  expect_equal(
    dv_scale_from_code(c(ode, "S2 <- V1/1000", "IPRED <- A_CENTRAL/S2")),
    1000
  )
})

test_that("nlmixr_pred_is_scaled: tells an applied scaling from none", {
  expect_true(nlmixr_pred_is_scaled("IPRED <- A_CENTRAL/S2"))
  expect_false(nlmixr_pred_is_scaled("IPRED <- A_CENTRAL/VC"))
  expect_false(nlmixr_pred_is_scaled("Y <- IPRED + IPRED*EPS1"))
})

# ── uncertainty draws keep the observation scaling ──────────────────────────

.nlmixr_model_code <- function(pop_cl = "POP_CL") {
  paste(c(
    "mod <- function() {",
    "  model({",
    paste0("    CL <- ", pop_cl, "*exp(ETA_CL)"),
    "    VC <- POP_VC*exp(ETA_VC)",
    "    d/dt(A_CENTRAL) = -CL*A_CENTRAL/VC",
    "    IPRED <- A_CENTRAL/VC",
    "    Y <- IPRED",
    "  })",
    "}"
  ), collapse = "\n")
}

## A model as `create_model(tool = "nlmixr", scale_observations = )` leaves it:
## `$code` without the scaling, the cached code with it.
.scaled_nlmixr_model <- function(scale = 1000) {
  base <- .nlmixr_model_code()
  structure(
    list(code = base),
    nlmixr_code = inject_nlmixr_scaling(base, scale)
  )
}

test_that("nlmixr_scale_observations: recovers the factor from the cached code", {
  expect_equal(nlmixr_scale_observations(.scaled_nlmixr_model(1000)), 1000)
  expect_equal(nlmixr_scale_observations(.scaled_nlmixr_model(500)), 500)
  ## nothing to recover
  expect_null(nlmixr_scale_observations(structure(
    list(code = .nlmixr_model_code()), nlmixr_code = .nlmixr_model_code()
  )))
  expect_null(nlmixr_scale_observations(list(code = .nlmixr_model_code())))
})

test_that("rerender_nlmixr_code: re-applies the scaling to a draw", {
  model <- .scaled_nlmixr_model(1000)
  ## the draw as `set_initial_estimates()` returns it: fresh `$code`, no
  ## cached attribute, and so no scaling
  draw <- list(code = .nlmixr_model_code("POP_CL_DRAW"))
  expect_equal(get_dv_scale_factor(code = draw$code), 1)

  code <- rerender_nlmixr_code(model, draw)
  expect_equal(get_dv_scale_factor(code = code), 1000)
  ## and it is still *this* draw's model
  expect_match(code, "POP_CL_DRAW", fixed = TRUE)
})

test_that("rerender_nlmixr_code: leaves an unscaled model unscaled", {
  model <- structure(
    list(code = .nlmixr_model_code()), nlmixr_code = .nlmixr_model_code()
  )
  code <- rerender_nlmixr_code(model, list(code = .nlmixr_model_code()))
  expect_equal(get_dv_scale_factor(code = code), 1)
  expect_false(nlmixr_pred_is_scaled(code))
})

test_that("rerender_nlmixr_code: does not scale a draw that is already scaled", {
  ## injecting over `IPRED <- A_CENTRAL/S1` would write `S1 <- S1/1000`
  model <- .scaled_nlmixr_model(1000)
  code <- rerender_nlmixr_code(model, list(code = attr(model, "nlmixr_code")))
  expect_equal(get_dv_scale_factor(code = code), 1000)
  expect_false(any(grepl("S1 <- S1", code_lines(code))))
})

test_that("rerender_nlmixr_code: carries the scaling along the whole chain", {
  ## create_model() -> mu_reference_model() -> update_parameters() (the fitted
  ## final model run_sim(fit = ) uses) -> set_initial_estimates() (a draw).
  ## Every step returns a fresh Pharmpy object whose `$code` has no scaling,
  ## so each one has to carry it over from the step before.
  built <- .scaled_nlmixr_model(1000)

  mu_referenced <- list(code = sub("ETA_CL", "ETA_CL + mu_1",
                                   .nlmixr_model_code(), fixed = TRUE))
  mu_code <- rerender_nlmixr_code(built, mu_referenced)
  expect_equal(get_dv_scale_factor(code = mu_code), 1000)

  fitted <- list(code = .nlmixr_model_code("POP_CL_FINAL"))
  final_code <- rerender_nlmixr_code(
    structure(mu_referenced, nlmixr_code = mu_code), fitted
  )
  expect_equal(get_dv_scale_factor(code = final_code), 1000)
  expect_match(final_code, "POP_CL_FINAL", fixed = TRUE)

  draw <- list(code = .nlmixr_model_code("POP_CL_DRAW"))
  draw_code <- rerender_nlmixr_code(
    structure(fitted, nlmixr_code = final_code), draw
  )
  expect_equal(get_dv_scale_factor(code = draw_code), 1000)
  expect_match(draw_code, "POP_CL_DRAW", fixed = TRUE)
})
