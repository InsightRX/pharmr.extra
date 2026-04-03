#' Set inter-individual variability on parameters
#'
#' @param mod pharmpy model object
#' @param iiv what parameters to put IIV on. Can be one of three formats:
#' - character: `all` or `basic`.
#' - character: `c("CL", "V")`. Will assume SD of 0.5 for initial estimate.
#' - list of numeric: e.g. `list(CL = 0.5, V = 0.5)` with SD for initial
#' estimates.
#' @param iiv_type one of IIV types accepted by pharmr::add_iiv(), i.e.
#' `add`, `prop`, `exp` (default), `log`, or `re_log`.
#'
set_iiv <- function(mod, iiv, iiv_type = "exp") {

  if(inherits(iiv, "character")) {
    pars <- get_defined_pk_parameters(mod)
    if(length(iiv) == 1 && iiv == "all") {
      iiv <- list()
      for(key in pars) iiv[[key]] <- 0.5
    } else if(length(iiv) == 1 && iiv == "basic") {
      iiv <- list(CL = 0.5)
      if("V" %in% pars) iiv$V <- 0.5
      if("V2" %in% pars) iiv$V2 <- 0.5
    } else { # assume user passed a vector of parameter names to put IIV on
      iiv_list <- list()
      for(key in iiv) iiv_list[[key]] <- 0.5
      iiv <- iiv_list
    }
  }

  if(!is.null(iiv)) {
    if(!inherits(iiv, "list")) {
      stop("`iiv` parameter should be a `list` or a `character` object.")
    }

    ## First remove all existing IIV
    ## Then, add univariate IIV (no BLOCKs yet)
    all_params <- get_defined_pk_parameters(mod)
    current <- get_parameters_with_iiv(mod)

    ## Map user-provided parameter names to actual model parameter names
    ## before computing set differences. Pharmpy's create_basic_pk_model uses
    ## different naming than the templates (e.g. VC instead of V, QP1 instead
    ## of Q, VP1 instead of V2, CLMM instead of CL for MM models).
    param_aliases <- list(
      V   = c("V1", "VC"),
      V1  = c("VC"),
      V2  = c("VP1"),
      V3  = c("VP2"),
      Q   = c("QP1"),
      Q2  = c("QP1"),
      Q3  = c("QP2"),
      CL  = c("CLMM")
    )
    ## Get all model assignments for fallback matching
    all_assignments <- tryCatch({
      stmts <- mod$statements$to_dict()$statements
      vapply(
        Filter(function(s) s$class == "Assignment", stmts),
        function(s) gsub("(Symbol\\(\\'|\\'\\))", "", s$symbol),
        character(1)
      )
    }, error = function(e) character(0))

    iiv_name_map <- stats::setNames(names(iiv), names(iiv))
    for(nm in names(iiv_name_map)) {
      if(stringr::str_detect(nm, "~")) next
      if(nm %in% names(param_aliases)) {
        ## Check if the original name is a real parameter (not just an alias)
        has_real_param <- nm %in% current ||
          (nm %in% all_assignments && length(mod$statements$find_assignment(nm)) > 0 &&
           !as.character(mod$statements$find_assignment(nm)$expression) %in% param_aliases[[nm]])
        if(!has_real_param) {
          for(alias in param_aliases[[nm]]) {
            if(alias %in% current || alias %in% all_assignments) {
              iiv_name_map[nm] <- alias
              break
            }
          }
        }
      }
    }
    ## Apply the mapping to iiv names (including correlation entries like "CL~V1")
    new_iiv_names <- names(iiv)
    for(i in seq_along(new_iiv_names)) {
      nm <- new_iiv_names[i]
      if(stringr::str_detect(nm, "~")) {
        parts <- stringr::str_split(nm, "~")[[1]]
        mapped_parts <- vapply(parts, function(p) {
          if(p %in% names(iiv_name_map)) iiv_name_map[p] else p
        }, character(1))
        new_iiv_names[i] <- paste(mapped_parts, collapse = "~")
      } else if(nm %in% names(iiv_name_map)) {
        new_iiv_names[i] <- iiv_name_map[nm]
      }
    }
    names(iiv) <- new_iiv_names

    ## Make sure iiv_type is a list (built after name mapping so keys match)
    if(inherits(iiv_type, "character")) {
      iiv_type_list <- list()
      for(key in names(iiv)) {
        iiv_type_list[[key]] <- iiv_type
      }
    } else {
      ## Map user-provided iiv_type keys to match mapped iiv names
      iiv_type_list <- iiv_type
      for(orig_nm in names(iiv_name_map)) {
        mapped_nm <- iiv_name_map[orig_nm]
        if(orig_nm != mapped_nm && orig_nm %in% names(iiv_type_list)) {
          iiv_type_list[[mapped_nm]] <- iiv_type_list[[orig_nm]]
          iiv_type_list[[orig_nm]] <- NULL
        }
      }
    }

    iiv_goal <- names(iiv)[!stringr::str_detect(names(iiv), "~")]
    iiv_corr <- names(iiv)[stringr::str_detect(names(iiv), "~")]
    has_corr <- unique(unlist(stringr::str_split(iiv_corr, "~")))
    to_remove <- setdiff(current, iiv_goal)
    to_reset <- intersect(iiv_goal, current)
    to_add <- setdiff(iiv_goal, current)
    map <- data.frame( # build a map for each parameter, whether it needs to be reset or not
      name = c(to_add, to_reset),
      reset = c(rep(FALSE, length(to_add)), rep(TRUE, length(to_reset)))
    ) |>
      dplyr::mutate(parameter = .data$name) |>
      dplyr::mutate(correlation = .data$name %in% has_corr) |>
      dplyr::arrange(.data$reset, .data$correlation) # make sure to first do the parameters that don't need a reset, to avoid creating DUMMYOMEGA
    for(i in seq_along(map$parameter)) {
      key <- map$name[i]
      par <- map$parameter[i]
      if(map$reset[match(par, map$parameter)]) {
        mod <- pharmr::remove_iiv(mod, par)
      }
      if(length(mod$statements$find_assignment(par)) > 0) {
        mod <- pharmr::add_iiv(
          model = mod,
          list_of_parameters = par,
          expression = iiv_type_list[[key]],
          initial_estimate = signif(iiv[[par]]^2, 5)
        )
      } else {
        cli::cli_alert_warning(paste0("Parameter declaration for ", key, " not found, cannot add IIV for ", key, "."))
      }
    }

    ## Then, if needed, change relevant $OMEGA to BLOCK
    ## Currently, pharmpy/pharmr does not support setting covariances currently
    ## so we'll write a custom function that just uses regex. It's a hacky solution
    ## but expectation is that pharmr will support this in the future.
    if(length(iiv_corr) > 0) {
      mod <- set_iiv_block(mod, iiv)
    }

  }
  mod
}

set_iiv_block <- function(
  model,
  iiv
) {

  ## make sure we have the IIV object in the same
  ## order as the IIVs in the NONMEM model
  pars <- get_parameters_with_iiv(model)
  iiv_ordered <- list()
  for(par in pars) {
    iiv_ordered[[par]] <- iiv[[par]]
    iiv[[par]] <- NULL
  }
  corr_params <- names(iiv)[grep("~", names(iiv))]
  for(par in corr_params) { # remainder of parameters
    iiv_ordered[[par]] <- iiv[[par]]
  }
  pars_with_corr <- intersect(
    names(iiv_ordered),
    unique(unlist(stringr::str_split(corr_params, "~")))
  )

  ## get omega lines, only the ones with correlations
  code <- stringr::str_split(model$code, "\\n")[[1]]
  omega_idx <- c()
  for(par in pars_with_corr) {
    idx <- grep(paste0("^\\$OMEGA .*? ; IIV_", par), code)
    omega_idx <- c(omega_idx, idx)
  }
  omega_lines <- code[omega_idx]

  ## Create the omega block
  om_block <- get_cov_matrix(
    iiv_ordered,
    nonmem = TRUE,
    limit = 0.001
  )
  omega <- c(
    glue::glue("$OMEGA BLOCK({length(om_block)})"),
    paste(om_block, paste0("; IIV_", pars_with_corr))
  )
  new_code <- c(
    code[1:(min(omega_idx)-1)],
    omega,
    code[(max(omega_idx)+1):length(code)]
  )
  ## Remove trailing blank lines that cause pharmpy DatasetError
  while(length(new_code) > 0 && new_code[length(new_code)] == "") {
    new_code <- new_code[-length(new_code)]
  }
  temp <- list(
    code = paste0(new_code, collapse = "\n"),
    dataset = model$dataset,
    datainfo = model$datainfo
  )
  new_model <- create_pharmpy_model_from_list(temp)
  new_model
}

#' Get a character vector with all parameters on which IIV is present
#'
#' @param mod TODO
get_parameters_with_iiv <- function(mod) {
  pars <- mod$random_variables$variance_parameters
  idx <- grep("IIV_", pars)
  eta_pars <- c()
  if(length(idx) > 0) {
    eta_pars <- pars[idx]
    eta_pars <- gsub("IIV_", "", eta_pars)
  }
  eta_pars
}

#' Get all parameters that are defined (from a predefined vector of possible parameters)
#'
#' @param mod TODO
#' @param possible TODO
get_defined_pk_parameters <- function(
    mod,
    possible = c("CL", "V", "V1", "V2", "V3", "Q", "Q2", "Q3", "K10", "K12", "K21", "K13", "K31")
) {
  pars <- c()
  statements <- mod$statements$to_dict()$statements
  for(i in seq(statements)) {
    obj <- statements[[i]]
    if(obj$class == "Assignment") {
      symbol <- gsub("(Symbol\\(\\'|\\'\\))", "", obj$symbol)
      if(symbol %in% possible) {
        pars <- c(pars, symbol)
      }
    }
  }
  pars
}

#' Function to set covariance between parameters in the omega block
#'
#' One caveat is that it will remove any existing covariances, since currently
#' there is no feature in pharmr/pharmpy to extract the covariance info.
#'
#' @param model TODO
#' @param covariance character vector specifying the parameters and initial
#' value for the correlation between the respective parameters, e.g.
#' `c("CL~V" = 0.1, "Q~V2" = 0.2)`.
#'
#' @returns Pharmpy model object
#'
#' @export
set_covariance <- function(model, covariance) {
  omegas <- pharmr::get_omegas(model)
  advan <- get_advan(model)
  params <- pharmr::get_pk_parameters(model)
  om_names <- omegas$names |>
    stringr::str_replace_all("IIV_", "")
  om_values <- lapply(omegas$inits, "sqrt") |>
    stats::setNames(om_names)
  ## remove existing covariance entries
  idx_cov <- !grepl("^OMEGA_", om_names)
  om_names <- om_names[idx_cov]
  om_values <- om_values[idx_cov]
  cov_terms <- c()
  for(key in names(covariance)) {
    if(stringr::str_detect(key, "\\~")) {
      om_values[[key]] <- covariance[[key]]
      terms <- stringr::str_split(key, "\\~")[[1]]
      cov_terms_safe <- lapply(terms, find_pk_parameter, model) |>
        as.character()
      names(om_values)[grepl(key, names(om_values))] <- paste(cov_terms_safe, collapse = "~")
      cov_terms <- c(cov_terms, cov_terms_safe)
    }
  }
  om_safe_names <- lapply(om_names, find_pk_parameter, model) |>
    as.character()
  ## Need to re-add parameters that are listed under a different name, e.g. V -> V2
  to_add <- setdiff(om_safe_names, om_names)
  model <- model |>
    pharmr::remove_iiv(to_add) |>
    pharmr::add_iiv(to_add, expression = "exp")
  if(! all(cov_terms %in% om_safe_names)) {
    cli::cli_abort("Cannot add covariance: no IIV is present on one or more of the parameters between which covariance is requested.")
  }
  names(om_values)[1:length(om_safe_names)] <- om_safe_names
  new_model <- set_iiv(
    model,
    iiv = om_values,
    iiv_type = "exp"
  )
  new_model
}
