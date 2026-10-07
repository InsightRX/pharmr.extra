#' Factor converting `dose / CL` into the concentration units a model reports
#'
#' NONMEM predicts `A(n) / S<n>` for the observation compartment, so a model
#' that reports ng/mL off an mg / L system writes `S2 = V2/1000`. AUC at steady
#' state *in those units* is `dose * (V / S) / CL`, not `dose / CL`, so without
#' this factor AUC_SS is off by exactly the scaling applied in the model (1000
#' in that example).
#'
#' @param model Pharmpy model object. `NULL` (or a model whose scaling cannot
#' be read) gives `1`.
#' @param verbose verbose output?
#' @param compartment observation compartment number. Inferred with
#' [get_obs_compartment()] when `NULL` (the default). NONMEM models only.
#' @param code nlmixr2 / rxode2 model code (character), read instead of
#' `model`. The scaling an nlmixr-format model applies lives only in its
#' rendered code — `create_model(scale_observations = )` rewrites the
#' prediction there as `S<n> <- <vol>/<scale>` — so the code is the only place
#' it can be read back from, and it is also all a worker process has.
#'
#' @details Returns `1` — i.e. plain `dose / CL` — whenever no scaling applies
#' or none can be read off the model:
#'
#' * no `S<n>` is defined for the observation compartment, in which case
#'   NONMEM scales by 1 and the model predicts amounts rather than
#'   concentrations,
#' * `S<n>` is the central volume itself (`S2 = V2`), the usual case,
#' * `S<n>` is something other than the central volume times or divided by a
#'   constant, which is reported as a warning since AUC_SS is then not
#'   `dose / CL` in any simple way,
#' * the model is an nlmixr-format Pharmpy model given without `code`: its
#'   statements carry no `S<n>` (see `code`).
#'
#' @returns single numeric multiplier.
#'
#' @export
get_dv_scale_factor <- function(
    model = NULL,
    compartment = NULL,
    code = NULL,
    verbose = FALSE
) {
  if(!is.null(code)) return(dv_scale_from_code(code, verbose = verbose))
  if(is.null(model)) return(1)

  ## An nlmixr-format model keeps its scaling in the rendered code rather than
  ## in its pharmpy statements, so it can only come in through `code`.
  tool <- tryCatch(get_tool_from_model(model), error = function(e) NA_character_)
  if(!identical(tool, "nonmem")) return(1)

  if(is.null(compartment)) {
    compartment <- tryCatch(
      suppressWarnings(get_obs_compartment(model)),
      error = function(e) NULL
    )
    if(is.null(compartment) || is.na(compartment)) return(1)
  }

  s_name <- paste0("S", compartment)
  s_expr <- tryCatch({
    assignment <- model$statements$find_assignment(s_name)
    if(is.null(assignment)) NULL else as.character(assignment$expression)
  }, error = function(e) NULL)
  ## No scaling record: NONMEM uses S = 1, so the prediction is an amount and
  ## no volume-based correction of dose/CL applies.
  if(is.null(s_expr) || is.na(s_expr)) return(1)

  parsed <- parse_dv_scale(s_expr)
  ## The factor is only `V / S` if the scaling really is the central volume
  ## over a constant: `S1 = V2/1000` on a two-compartment model parses just as
  ## cleanly, and its factor is `1000 * V1/V2`, which varies by subject.
  if(!is.null(parsed) &&
     ! parsed$variable %in% central_volume_names(model, parsed$variable)) {
    parsed <- NULL
  }
  if(is.null(parsed)) {
    cli::cli_warn(c(
      "Could not interpret the scaling {.code {s_name} = {s_expr}} as the \\
       central volume times or divided by a constant.",
      i = "AUC_SS is reported as dose/CL, which is only in the units of the \\
           simulated concentrations when {.code {s_name}} is the central volume."
    ))
    return(1)
  }

  if(verbose && !isTRUE(all.equal(parsed$factor, 1))) {
    cli::cli_alert_info(
      "Scaling AUC_SS by {parsed$factor} to match {.code {s_name} = {s_expr}}."
    )
  }
  parsed$factor
}

#' Names the central volume goes by in a NONMEM model
#'
#' From the model's topology, via `pharmr::get_central_volume_and_clearance()`
#' — the same source `create_model(scale_observations = )` writes the scaling
#' from. `find_pk_parameter("V", )` is only a fallback for models it cannot
#' read: that one guesses from ADVAN, and on an ODE model it answers `V2` (a
#' peripheral volume) for a model whose central volume is `V1`.
#'
#' @inheritParams get_dv_scale_factor
#' @param scaled_with the variable the scaling is written in terms of, checked
#' for being an alias of the central volume (pharmpy emits `V = VC` and may
#' scale either name).
#'
#' @returns character vector of names that denote the central volume.
#' @noRd
central_volume_names <- function(model, scaled_with = NULL) {
  central <- tryCatch(
    as.character(pharmr::get_central_volume_and_clearance(model)[[1]]),
    error = function(e) NULL
  )
  if(length(central) == 0) {
    central <- tryCatch(
      suppressMessages(find_pk_parameter("V", model)),
      error = function(e) NULL
    )
  }
  central <- unique(central[!is.na(central) & nzchar(central)])
  ## Nothing to go on: fall back to the names pharmpy and this package
  ## generate, rather than refusing every model whose topology cannot be read.
  if(length(central) == 0) return(c("V", "V1", "V2", "VC"))

  ## One level of aliasing, either way round.
  expression_of <- function(name) {
    tryCatch({
      assignment <- model$statements$find_assignment(name)
      if(is.null(assignment)) NULL else as.character(assignment$expression)
    }, error = function(e) NULL)
  }
  aliases <- unlist(lapply(central, expression_of))
  if(!is.null(scaled_with) && length(expression_of(scaled_with)) > 0 &&
     expression_of(scaled_with) %in% central) {
    aliases <- c(aliases, scaled_with)
  }
  unique(c(central, aliases))
}

#' Read the observation scaling off nlmixr2 / rxode2 model code
#'
#' The prediction is emitted as `IPRED <- A_CENTRAL/<denominator>`. A model
#' with no unit scaling divides by the volume directly; one built with
#' `create_model(scale_observations = )` divides by `S<n>` and defines
#' `S<n> <- <vol>/<scale>` above it (see `inject_nlmixr_scaling()`), which is
#' the NONMEM `S<n> = V/scale` convention. Only the latter needs a factor.
#'
#' @param code model code, as a single string or as lines.
#' @param verbose report the factor when it is not 1?
#'
#' @returns single numeric multiplier.
#' @noRd
dv_scale_from_code <- function(code, verbose = FALSE) {
  lines <- unlist(strsplit(paste(as.character(code), collapse = "\n"), "\n",
                           fixed = TRUE))
  var <- "[A-Za-z][A-Za-z0-9_]*"
  pred_re <- paste0(
    "^\\s*(?:IPRED|F)\\s*(?:<-|=)\\s*", var,
    "(?:\\([0-9]+\\))?\\s*/\\s*(", var, ")\\s*$"
  )
  denominator <- .first_capture(lines, pred_re)
  ## No prediction of that shape, or it divides by the volume directly: no
  ## scaling is being applied.
  if(is.null(denominator) || !grepl("^S[0-9]+$", denominator)) return(1)

  s_expr <- .first_capture(
    lines, paste0("^\\s*", denominator, "\\s*(?:<-|=)\\s*(.+?)\\s*$")
  )
  if(is.null(s_expr)) return(1)

  ## Assembled rather than interpolated piecewise: cli reads `<-` inside
  ## inline markup as the start of a tag.
  statement <- paste(denominator, "<-", s_expr)
  parsed <- parse_dv_scale(s_expr)
  ## The factor is only `V / S` if the scaling really is the central volume
  ## over a constant. `S2 <- WT/1000` parses just as well and is not a unit
  ## conversion at all, so it has to be rejected rather than applied.
  if(!is.null(parsed) && ! parsed$variable %in% volume_names_in_code(lines)) {
    parsed <- NULL
  }
  if(is.null(parsed)) {
    cli::cli_warn(c(
      "Could not interpret the scaling {.code {statement}} as the central \\
       volume times or divided by a constant.",
      i = "AUC_SS is reported as dose/CL, which is only in the units of the \\
           simulated concentrations when {.code {denominator}} is the central \\
           volume."
    ))
    return(1)
  }
  if(verbose && !isTRUE(all.equal(parsed$factor, 1))) {
    cli::cli_alert_info(
      "Scaling AUC_SS by {parsed$factor} to match {.code {statement}}."
    )
  }
  parsed$factor
}

#' Names the central volume can go by in nlmixr2 / rxode2 model code
#'
#' There is no model object to ask on this path (a worker process has the code
#' and nothing else), so the volume is identified from the code itself: what
#' the central compartment eliminates through in `d/dt(A_CENTRAL)`, plus
#' anything aliased to that — pharmpy emits `V <- VC` and then scales `VC`.
#' Only where the ODE yields nothing do the names pharmpy and this package
#' generate stand in. Rejecting a name only costs a warning and the old
#' unscaled `dose / CL`.
#'
#' @param lines model code, as lines.
#'
#' @returns character vector of candidate volume names.
#' @noRd
volume_names_in_code <- function(lines) {
  var <- "[A-Za-z][A-Za-z0-9_]*"
  amount <- "A_[A-Za-z0-9_]+"
  ## `CL*A_CENTRAL/V`, `A_CENTRAL*CL/V`, or `(CL/V)*A_CENTRAL`
  from_ode <- c(
    .first_capture(lines, paste0(
      "^.*\\bCL\\s*\\*\\s*", amount, "\\s*/\\s*(", var, ")\\b.*$"
    )),
    .first_capture(lines, paste0(
      "^.*\\b", amount, "\\s*\\*\\s*CL\\s*/\\s*(", var, ")\\b.*$"
    )),
    .first_capture(lines, paste0(
      "^.*\\bCL\\s*/\\s*(", var, ")\\s*\\)?\\s*\\*\\s*", amount, ".*$"
    ))
  )
  ## One level of aliasing, both ways round: `V <- VC` makes either name the
  ## volume, whichever of the two the ODE uses.
  aliases <- unlist(lapply(from_ode, function(volume) {
    alias_re <- paste0("^\\s*(", var, ")\\s*(?:<-|=)\\s*(", var, ")\\s*$")
    hits <- regmatches(lines, regexec(alias_re, lines, perl = TRUE))
    hits <- hits[lengths(hits) > 2]
    sides <- lapply(hits, function(hit) {
      if(hit[2] == volume) hit[3] else if(hit[3] == volume) hit[2] else NULL
    })
    unlist(sides)
  }))
  from_ode <- unique(c(from_ode, aliases))
  ## Only where the ODE says nothing: with explicit evidence that elimination
  ## runs through `VC`, a scaling written in terms of `V2` is a peripheral
  ## volume rather than a unit conversion, and generic names must not override
  ## that.
  if(length(from_ode) > 0) return(from_ode)
  c("V", "V1", "V2", "VC")
}

#' First capture group matched by `pattern` over `lines`, or `NULL`
#' @noRd
.first_capture <- function(lines, pattern) {
  hits <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  hits <- hits[lengths(hits) > 1]
  if(length(hits) == 0) return(NULL)
  hits[[1]][2]
}

#' Read a compartment scaling expression as a variable and a scaling factor
#'
#' The factor is the multiplier on `dose / CL`, i.e. `V / S`: `V/1000` scales
#' concentrations (and so AUC) up by 1000, `V*1000` down by 1000.
#'
#' @param expression scaling expression as a string, e.g. `"V2/1000"`.
#'
#' @returns a list with `variable` and `factor`, or `NULL` when the expression
#' is not a bare variable optionally times or divided by a positive constant.
#' @noRd
parse_dv_scale <- function(expression) {
  expr <- gsub("[[:space:]]", "", as.character(expression))
  var <- "([A-Za-z][A-Za-z0-9_]*)"
  num <- "([0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)"
  patterns <- list(
    ## S = V
    list(re = paste0("^", var, "$"),            v = 2, f = NA, inv = FALSE),
    ## S = V/1000  ->  concentrations 1000x the amount/volume units
    list(re = paste0("^", var, "/", num, "$"),  v = 2, f = 3,  inv = FALSE),
    ## S = V*1000 / S = 1000*V  ->  1000x smaller
    list(re = paste0("^", var, "\\*", num, "$"), v = 2, f = 3, inv = TRUE),
    list(re = paste0("^", num, "\\*", var, "$"), v = 3, f = 2, inv = TRUE)
  )
  for(p in patterns) {
    m <- regmatches(expr, regexec(p$re, expr, perl = TRUE))[[1]]
    if(length(m) == 0) next
    factor <- if(is.na(p$f)) 1 else as.numeric(m[p$f])
    if(is.na(factor) || !is.finite(factor) || factor <= 0) return(NULL)
    if(p$inv) factor <- 1 / factor
    return(list(variable = m[p$v], factor = factor))
  }
  NULL
}
