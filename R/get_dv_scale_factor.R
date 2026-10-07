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
  volumes <- c(
    tryCatch(suppressMessages(find_pk_parameter("V", model)),
             error = function(e) NULL),
    "V", "V1", "V2", "VC"
  )
  if(is.null(parsed) || ! parsed$variable %in% volumes) {
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
