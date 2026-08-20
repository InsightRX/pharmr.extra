#' Add a `$PRIOR NWPRI` record set to a NONMEM model
#'
#' Builds the NONMEM normal-inverse-Wishart prior records (`$PRIOR NWPRI`,
#' `$THETAP`, `$THETAPV`, `$OMEGAP`, `$OMEGAPD`, `$SIGMAP`, `$SIGMAPD`) that
#' let NONMEM itself draw parameter vectors from a fit's uncertainty
#' distribution, and inserts them into the model code.
#'
#' Combined with `$SIMULATION ... TRUE=PRIOR` (see
#' [set_simulation_clean()]) this is the `uncertainty_engine = "nwpri"` route
#' of [run_sim()]: NONMEM draws a fresh parameter vector for every simulation
#' subproblem, so `n` uncertainty replicates cost one NONMEM compile rather
#' than `n`.
#'
#' @section How the prior is parameterised:
#'
#' The prior is centred on the fit, so simulating from it reproduces the fit's
#' parameter uncertainty rather than adding information to it.
#'
#' * `$THETAP` holds the THETA point estimates and `$THETAPV` the THETA block
#'   of the covariance matrix, i.e. a multivariate normal prior on THETA.
#' * `$OMEGAP` / `$SIGMAP` hold the OMEGA and SIGMA point estimates, mirroring
#'   the block structure of the model's own `$OMEGA` / `$SIGMA` records.
#' * `$OMEGAPD` / `$SIGMAPD` hold the inverse-Wishart degrees of freedom, one
#'   per block, chosen so the prior variance of each block matches the
#'   estimated standard error: `df = 2 * mean((estimate / se)^2) + p` over the
#'   block's `p` diagonal elements (Gisleskog, Karlsson and Beal 2002; the
#'   same rule PsN uses, which for the usual `p = 1` diagonal element reduces
#'   to `2 * (estimate / se)^2 + 1`).
#'
#' Standard errors are taken as the square root of the covariance matrix
#' diagonal rather than from `fit$standard_errors`, so the emitted records are
#' internally consistent with the covariance matrix they came from.
#'
#' Because NWPRI treats the THETA prior, each OMEGA block and each SIGMA block
#' as independent, the THETA-OMEGA and THETA-SIGMA covariances that
#' `$COVARIANCE` reports are *not* carried into the prior. See the
#' `uncertainty_engine` section of [run_sim()] for what that means in practice.
#'
#' @section Parameters not covered by the covariance matrix:
#'
#' Fixed parameters, and any other parameter the covariance matrix does not
#' cover, cannot be given an uncertainty. They still have to appear in the
#' prior records (NONMEM requires the prior to mirror the model's parameter
#' structure), so they are emitted with a negligible prior variance
#' (a relative standard deviation of `1e-3`) and are therefore held at their
#' point estimate for all practical purposes. A warning lists them.
#'
#' @param model a Pharmpy NONMEM model object.
#' @param fit a Pharmpy modelfit object carrying `parameter_estimates` and
#' `covariance_matrix`. Ignored when `parameter_estimates` and
#' `covariance_matrix` are given directly.
#' @param plev probability mass the THETA draws are truncated to, emitted as
#' `$PRIOR NWPRI PLEV=`. NONMEM has no default that works with
#' `TRUE=PRIOR` (it stops with `VALUE OF ARGUMENT 'PLEV' IS INAPPROPRIATE`),
#' so this is always written out. The default is close enough to 1 that
#' truncation is negligible.
#' @param parameter_estimates named numeric vector of point estimates,
#' overriding `fit$parameter_estimates`.
#' @param covariance_matrix parameter uncertainty covariance matrix with
#' parameter names as row/column names, overriding `fit$covariance_matrix`.
#'
#' @returns a Pharmpy NONMEM model object with the prior records added.
#'
#' @examples
#' \dontrun{
#' fit <- run_nlme(model = model, data = data)
#' prior_model <- add_nwpri_prior(attr(fit, "final_model"), fit)
#' }
#'
#' @export
add_nwpri_prior <- function(
    model,
    fit = NULL,
    plev = 0.9999,
    parameter_estimates = NULL,
    covariance_matrix = NULL
) {
  if(!inherits(model, "pharmpy.model.model.Model")) {
    cli::cli_abort("`model` must be a Pharmpy model object.")
  }
  if(get_tool_from_model(model) != "nonmem") {
    cli::cli_abort(c(
      "{.code $PRIOR NWPRI} is a NONMEM feature.",
      x = "The supplied model is not a NONMEM model."
    ))
  }
  if(is.null(parameter_estimates)) parameter_estimates <- fit$parameter_estimates
  if(is.null(covariance_matrix))   covariance_matrix   <- fit$covariance_matrix
  if(is.null(parameter_estimates) || is.null(covariance_matrix)) {
    cli::cli_abort(c(
      "{.fn add_nwpri_prior} needs a `fit` with a covariance matrix.",
      i = "Run the model with a {.code $COVARIANCE} step (or SIR) so parameter \\
           uncertainty can be turned into a prior."
    ))
  }

  ## Keep the dataset: reading the model back from a string drops the
  ## in-memory dataset, the same way `restore_table_records()` handles it.
  data <- model$dataset
  param_structure <- nwpri_model_structure(model)
  records <- build_nwpri_records(
    param_structure     = param_structure,
    parameter_estimates = parameter_estimates,
    covariance_matrix   = covariance_matrix,
    plev                = plev
  )
  code <- insert_nwpri_records(model$code, records)
  model <- pharmr::read_model_from_string(code = code)
  if(!is.null(data)) {
    model <- pharmr::set_dataset(model, path_or_df = data)
  }
  model
}

#' Describe a NONMEM model's THETA/OMEGA/SIGMA structure by parameter name
#'
#' `$OMEGAP` and `$SIGMAP` have to mirror the block structure of the model's
#' own `$OMEGA` / `$SIGMA` records, so the prior needs to know which variance
#' parameters sit in which block. Pharmpy models that structure as one
#' distribution per block, which is what this reads back out.
#'
#' @param model a Pharmpy NONMEM model object.
#'
#' @returns list with `theta` (character vector of THETA parameter names, in
#' declaration order), and `omega` / `sigma`: lists of square character
#' matrices, one per block, holding the parameter name of each element.
#' @noRd
nwpri_model_structure <- function(model) {
  rvs <- model$random_variables

  block_names <- function(dist) {
    names <- as.character(unlist(reticulate::py_to_r(dist$names)))
    p <- length(names)
    m <- matrix("", nrow = p, ncol = p)
    for(i in seq_len(p)) {
      for(j in seq_len(p)) {
        m[i, j] <- reticulate::py_str(rvs$get_covariance(names[i], names[j]))
      }
    }
    m
  }

  eta_names <- as.character(unlist(reticulate::py_to_r(rvs$etas$names)))
  omega <- list()
  sigma <- list()
  for(dist in reticulate::iterate(rvs)) {
    m <- block_names(dist)
    first <- as.character(unlist(reticulate::py_to_r(dist$names)))[1]
    if(first %in% eta_names) {
      omega[[length(omega) + 1L]] <- m
    } else {
      sigma[[length(sigma) + 1L]] <- m
    }
  }

  ## Whatever is left after the variance parameters is a THETA. Keeping the
  ## model's own parameter order matters: `$THETAP` is positional, so entry `i`
  ## must be THETA(i).
  variance_params <- unique(c(unlist(omega), unlist(sigma)))
  all_params <- as.character(unlist(reticulate::py_to_r(model$parameters$names)))
  theta <- setdiff(all_params, variance_params)

  list(theta = theta, omega = omega, sigma = sigma)
}

#' Relative standard deviation given to parameters with no uncertainty
#'
#' They cannot be left out of the prior records (NONMEM requires the prior to
#' mirror the model's parameter structure), so they get a variance small
#' enough to be irrelevant instead. Not zero: a zero variance would make
#' `$THETAPV` singular.
#' @noRd
nwpri_fixed_rsd <- function() 1e-3

#' Correlation used in place of a zero covariance inside a prior BLOCK record
#' @noRd
nwpri_zero_cov_rho <- function() 1e-6

#' Replace exactly-zero off-diagonal covariances in a prior BLOCK record
#'
#' NM-TRAN rejects a `BLOCK()` record that contains a zero covariance unless
#' the zero pattern happens to make it a band matrix
#' (`64 A COVARIANCE IS ZERO, BUT THE BLOCK IS NOT A BAND MATRIX`). Zeros turn
#' up here whenever a parameter has no uncertainty to share with the others —
#' a FIXED THETA, say — so rather than work out whether a given zero pattern
#' happens to be legal, give those pairs a negligible correlation instead. At
#' `1e-6` that is not a statement about the parameters; it is a statement about
#' NM-TRAN.
#'
#' @param m symmetric covariance matrix.
#'
#' @returns `m` with its zero off-diagonal elements replaced.
#' @noRd
nwpri_fill_zero_covariances <- function(m) {
  if(nrow(m) < 2) return(m)
  rho <- nwpri_zero_cov_rho()
  for(i in seq_len(nrow(m))) {
    for(j in seq_len(i - 1L)) {
      if(m[i, j] == 0) {
        m[i, j] <- m[j, i] <- rho * sqrt(m[i, i] * m[j, j])
      }
    }
  }
  m
}

#' Format a number the way NONMEM prior records want it
#' @noRd
nwpri_fmt <- function(x) formatC(as.numeric(x), format = "e", digits = 6)

#' Lower-triangle rows of a matrix, as NONMEM `BLOCK()` record lines
#' @noRd
nwpri_lower_triangle <- function(m) {
  vapply(
    seq_len(nrow(m)),
    function(i) paste(nwpri_fmt(m[i, seq_len(i)]), collapse = " "),
    character(1)
  )
}

#' Inverse-Wishart degrees of freedom matching a block's estimated precision
#'
#' NONMEM parameterises the `$OMEGAP` / `$SIGMAP` prior by its mean and a
#' degrees-of-freedom value, under which the prior standard deviation of a
#' diagonal element is approximately `estimate * sqrt(2 / (df - p))` for a
#' block of dimension `p`. Inverting that for each diagonal element and
#' averaging gives a single df for the block, which is all NONMEM accepts.
#'
#' For the usual `p = 1` case this is exactly the `2 * (estimate / se)^2 + 1`
#' rule of Gisleskog, Karlsson and Beal (2002).
#'
#' @param estimates numeric vector of the block's diagonal point estimates.
#' @param ses numeric vector of the matching standard errors.
#'
#' @returns a single numeric degrees-of-freedom value.
#' @noRd
nwpri_block_df <- function(estimates, ses) {
  p <- length(estimates)
  mean(2 * (estimates / ses)^2) + p
}

#' Build the NWPRI prior records for a model
#'
#' Pure string construction: everything Pharmpy-specific has already been
#' resolved into `param_structure` by [nwpri_model_structure()], so this is
#' testable without Python.
#'
#' @param param_structure output of [nwpri_model_structure()].
#' @param parameter_estimates named numeric vector (or pandas Series) of point
#' estimates.
#' @param covariance_matrix parameter uncertainty covariance matrix with
#' parameter names as row/column names.
#' @param plev `PLEV=` value for the `$PRIOR` record.
#'
#' @returns list with `prior` (the single `$PRIOR NWPRI` line) and `records`
#' (character vector of the `$THETAP` ... `$SIGMAPD` lines).
#' @noRd
build_nwpri_records <- function(
    param_structure,
    parameter_estimates,
    covariance_matrix,
    plev = 0.9999
) {
  if(!is.numeric(plev) || length(plev) != 1 || is.na(plev) ||
     plev <= 0 || plev >= 1) {
    cli::cli_abort("`plev` must be a single number strictly between 0 and 1.")
  }

  to_r <- function(x) {
    if(inherits(x, "python.builtin.object")) reticulate::py_to_r(x) else x
  }
  est <- to_r(parameter_estimates)
  est_names <- names(est)
  est <- stats::setNames(as.numeric(est), est_names)
  if(is.null(est_names)) {
    cli::cli_abort("`parameter_estimates` must be a named vector/Series of parameters.")
  }

  cov_mat <- as.matrix(to_r(covariance_matrix))
  storage.mode(cov_mat) <- "double"
  cov_names <- colnames(cov_mat)
  if(is.null(cov_names)) {
    cli::cli_abort("`covariance_matrix` must have parameter names as row/column names.")
  }
  row_names <- rownames(cov_mat)
  if(is.null(row_names)) {
    rownames(cov_mat) <- cov_names
  } else if(!setequal(row_names, cov_names)) {
    cli::cli_abort("`covariance_matrix` row and column names must reference the same parameters.")
  } else if(!identical(row_names, cov_names)) {
    cov_mat <- cov_mat[cov_names, , drop = FALSE]
  }

  blocks <- c(param_structure$omega, param_structure$sigma)
  ## `$OMEGA BLOCK(n) SAME` (IOV) makes pharmpy report one distribution per
  ## occurrence, all sharing the same parameters. NWPRI has no way to express
  ## "and this block equals the previous one", so a prior built from that
  ## structure would sample each occurrence independently and quietly break the
  ## SAME constraint. Refuse rather than emit it.
  shared <- table(unlist(lapply(blocks, function(b) unique(as.vector(b)))))
  repeated <- names(shared)[shared > 1]
  if(length(repeated) > 0) {
    cli::cli_abort(c(
      "{.fn add_nwpri_prior} does not support models whose OMEGA/SIGMA blocks \\
       share parameters.",
      x = "Shared across blocks: {repeated}",
      i = "This is what {.code $OMEGA BLOCK(n) SAME} (IOV) looks like, and \\
           {.code $PRIOR NWPRI} cannot express the SAME constraint: each \\
           occurrence would be sampled independently."
    ))
  }

  model_params <- unique(c(param_structure$theta, unlist(blocks)))
  missing_est <- setdiff(model_params, est_names)
  if(length(missing_est) > 0) {
    cli::cli_abort(c(
      "`parameter_estimates` is missing model parameters needed for the prior.",
      x = "Missing: {missing_est}"
    ))
  }

  ## The inverse-Wishart prior on an OMEGA/SIGMA block needs a positive
  ## definite scale, so a variance estimated (or fixed) at zero — a dummy ETA,
  ## typically — has no prior to give it.
  variance_diag <- unlist(lapply(blocks, diag))
  non_positive <- variance_diag[est[variance_diag] <= 0]
  if(length(non_positive) > 0) {
    cli::cli_abort(c(
      "{.fn add_nwpri_prior} needs every OMEGA/SIGMA variance to be positive.",
      x = "Estimated at zero or below: {non_positive}",
      i = "{.code $PRIOR NWPRI} draws these from an inverse-Wishart \\
           distribution, which has no zero-variance case. Remove the \\
           parameter from the model, or use \\
           {.code uncertainty_engine = \"replicates\"}."
    ))
  }

  ## Parameters without an uncertainty (fixed, or simply absent from the
  ## covariance step) are kept at their estimate rather than dropped; see the
  ## "Parameters not covered by the covariance matrix" section of
  ## `add_nwpri_prior()`. A zero variance on the covariance diagonal means the
  ## same thing: the parameter was reported but not actually estimated.
  covered <- cov_names[diag(cov_mat) > 0]
  held_fixed <- setdiff(model_params, covered)
  if(length(held_fixed) > 0) {
    cli::cli_warn(c(
      "!" = "Covariance matrix does not cover all model parameters; \\
             {length(held_fixed)} parameter{?s} given a negligible prior \\
             variance (uncertainty not propagated).",
      "i" = "Held fixed: {held_fixed}"
    ))
  }
  ## The standard errors the degrees of freedom are derived from. Taken from
  ## the covariance diagonal so the prior is consistent with the matrix it was
  ## built from even for a fit whose reported standard errors came from
  ## elsewhere (e.g. SIR).
  se_of <- function(p) {
    if(p %in% covered) sqrt(cov_mat[p, p]) else abs(est[[p]]) * nwpri_fixed_rsd()
  }

  out <- character(0)

  ## $THETAP / $THETAPV ------------------------------------------------------
  thetas <- param_structure$theta
  if(length(thetas) > 0) {
    out <- c(out, paste0(
      "$THETAP ",
      paste0("(", nwpri_fmt(est[thetas]), " FIX)", collapse = " ")
    ))
    tv <- matrix(0, nrow = length(thetas), ncol = length(thetas),
                 dimnames = list(thetas, thetas))
    estimated <- intersect(thetas, covered)
    if(length(estimated) > 0) {
      tv[estimated, estimated] <- cov_mat[estimated, estimated, drop = FALSE]
    }
    for(p in setdiff(thetas, covered)) {
      ## Positive definiteness: a zero diagonal would make $THETAPV singular.
      tv[p, p] <- max((abs(est[[p]]) * nwpri_fixed_rsd())^2, .Machine$double.eps)
    }
    out <- c(
      out,
      paste0("$THETAPV BLOCK(", length(thetas), ") FIX"),
      nwpri_lower_triangle(nwpri_fill_zero_covariances(tv))
    )
  }

  ## $OMEGAP / $OMEGAPD and $SIGMAP / $SIGMAPD -------------------------------
  emit_variance_prior <- function(blocks, prefix) {
    if(length(blocks) == 0) return(character(0))
    lines <- character(0)
    dfs <- numeric(length(blocks))
    for(k in seq_along(blocks)) {
      b <- blocks[[k]]
      p <- nrow(b)
      vals <- matrix(est[as.vector(b)], nrow = p, ncol = p)
      if(p == 1) {
        lines <- c(lines, paste0("$", prefix, "P ", nwpri_fmt(vals[1, 1]), " FIX"))
      } else {
        lines <- c(
          lines,
          paste0("$", prefix, "P BLOCK(", p, ") FIX"),
          nwpri_lower_triangle(nwpri_fill_zero_covariances(vals))
        )
      }
      diag_names <- diag(b)
      dfs[k] <- nwpri_block_df(
        estimates = est[diag_names],
        ses       = vapply(diag_names, se_of, numeric(1))
      )
    }
    c(lines, paste0(
      "$", prefix, "PD ",
      paste0("(", nwpri_fmt(dfs), " FIX)", collapse = " ")
    ))
  }

  out <- c(out, emit_variance_prior(param_structure$omega, "OMEGA"))
  out <- c(out, emit_variance_prior(param_structure$sigma, "SIGMA"))

  list(
    prior   = paste0("$PRIOR NWPRI PLEV=", plev),
    records = out
  )
}

#' Records a `$PRIOR` line must be placed before
#'
#' NM-TRAN wants `$PRIOR` after the problem/data set-up and before the
#' parameter records; the first of these that appears is the anchor.
#' @noRd
nwpri_prior_anchor_records <- c("MODEL", "PK", "PRED", "DES", "ERROR", "THETA")

#' Records the prior parameter block is placed after
#' @noRd
nwpri_parameter_records <- c("THETA", "OMEGA", "SIGMA")

#' Insert NWPRI prior records into NONMEM model code
#'
#' @param code NONMEM model code (single string or character vector of lines).
#' @param records output of [build_nwpri_records()].
#'
#' @returns NONMEM model code as a single string.
#' @noRd
insert_nwpri_records <- function(code, records) {
  lines <- unlist(stringr::str_split(code, "\n"))
  starts <- grep("^\\s*\\$", lines)
  if(length(starts) == 0) {
    cli::cli_abort("No NONMEM records found in the model code.")
  }
  names_at <- toupper(
    stringr::str_replace(
      stringr::str_extract(lines[starts], "^\\s*\\$[A-Za-z]+"),
      "^\\s*\\$", ""
    )
  )

  if(any(names_at == "PRIOR")) {
    cli::cli_abort(c(
      "The model already has a {.code $PRIOR} record.",
      i = "{.fn add_nwpri_prior} refuses to add a second one; remove the \\
           existing prior first."
    ))
  }

  ## Prior parameter records go after the last $THETA/$OMEGA/$SIGMA record,
  ## i.e. immediately before whatever record follows it.
  param_at <- which(names_at %in% nwpri_parameter_records)
  if(length(param_at) == 0) {
    cli::cli_abort("The model has no $THETA/$OMEGA/$SIGMA records to build a prior from.")
  }
  last_param <- max(param_at)
  after_last_param <- if(last_param == length(starts)) {
    length(lines) + 1L
  } else {
    starts[last_param + 1L]
  }

  ## $THETA is one of the anchors, so this is never empty once the model has
  ## the parameter records checked for above.
  anchor_at <- which(names_at %in% nwpri_prior_anchor_records)
  prior_line_at <- starts[min(anchor_at)]

  ## Insert from the back so the earlier index stays valid.
  lines <- append(lines, c(records$records, ""), after = after_last_param - 1L)
  lines <- append(lines, c(records$prior, ""), after = prior_line_at - 1L)
  paste(lines, collapse = "\n")
}

#' Does a NONMEM model (object or code) carry a `$PRIOR` record?
#'
#' @param model a Pharmpy NONMEM model object, or NONMEM model code.
#'
#' @returns `TRUE` or `FALSE`.
#'
#' @export
has_prior_record <- function(model) {
  code <- if(inherits(model, "pharmpy.model.model.Model")) model$code else model
  any(grepl("^\\s*\\$PRIOR", unlist(stringr::str_split(code, "\n"))))
}
