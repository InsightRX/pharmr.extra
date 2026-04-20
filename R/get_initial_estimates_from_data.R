#' Get a very crude estimate for V to serve as initial estimate
#' for CL and V, without performing an NCA. The calculation is based on
#' the assumption that often in clinical trial data, there is at least a
#' peak and a trough (and likely other samples) taken, hence it's
#' possible to get a crude estimate for CL and V from that.
#' For 2-compartment models we just set Q and V to half and
#' twice the size of CL and V, which is often a good starting point.
#' In most scenarios this is sufficiently close to the final estimates that
#' estimation methods will be able to find the global minimum.
#'
#' @param data NONMEM-style dataset
#' @param n_cmt number of distribution / elimination compartments.
#' @param scale_observations TODO
#' @param ltbs is DV column log-transformed?
#'
#' @returns TODO
#' 
#' @export
get_initial_estimates_from_data <- function(
  data,
  n_cmt = 1,
  scale_observations = NULL,
  ltbs = FALSE
) {

  data <- load_data_wrapper(data)

  ## Pre-process once for all individuals instead of repeating per-individual
  suppressWarnings(
    data <- data |>
      dplyr::mutate(
        AMT = as.numeric(as.character(.data$AMT)),
        DV = as.numeric(.data$DV),
        TIME = as.numeric(.data$TIME)
      ) |>
      dplyr::group_by(.data$ID) |>
      dplyr::mutate(dosenr = cumsum(.data$EVID)) |>
      dplyr::ungroup()
  )
  if(ltbs) {
    data <- data |>
      dplyr::mutate(DV = ifelse(.data$EVID == 0, exp(.data$DV), .data$DV))
  }

  ## TODO: an extension could be to automatically add
  ## observed value scaling, e.g. when V < 1.0.
  id_data <- split(data, data$ID)
  pars_list <- vector("list", length(id_data))
  for(i in seq_along(id_data)) {
    tmp <- get_initial_estimates_from_individual_data(
      id_data[[i]],
      ltbs = FALSE  # already applied above
    )
    if(length(tmp) > 0) {
      pars_list[[i]] <- tmp
    }
  }
  pars <- dplyr::bind_rows(pars_list)
  tmp <- pars |>
    # remove all Inf values and replace them with the maximum value for the column
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.infinite(.), max(.[!is.infinite(.)], na.rm = TRUE), .))) 
  est <- apply(tmp[,1:2], 2, function(x) { 
    signif(weighted.mean(x, w = tmp$weight, na.rm=TRUE), 3)
  })
  if(n_cmt >= 2) {
    est$QP1 <- est$CL
    est$VP1 <- est$V * 2
  }
  if(n_cmt == 3) {
    est$QP2 <- est$CL
    est$VP2 <- est$V * 3
  }
  if(is.null(scale_observations)) {
    scale_observations <- 1
  }
  for(i in seq_along(est)) {
    est[[i]] <- signif(est[[i]] * scale_observations, 3)
  }

  est
}

#' Core function to get parameter estimates from individual data
#'
#' @param inheritParams get_initial_estimates_from_data
#' 
get_initial_estimates_from_individual_data <- function(
  data,
  ltbs = FALSE
) {

  dat <- data

  ## Ensure required columns are prepared (outer function does this for bulk
  ## calls, but handle here too so the function works when called directly)
  if(!is.numeric(dat$DV))   dat$DV   <- suppressWarnings(as.numeric(dat$DV))
  if(!is.numeric(dat$TIME)) dat$TIME <- suppressWarnings(as.numeric(dat$TIME))
  if(is.null(dat$dosenr))   dat$dosenr <- cumsum(dat$EVID)

  if(ltbs) {
    dat$DV <- ifelse(dat$EVID == 0, exp(dat$DV), dat$DV)
  }

  ## Get first dose number for which more than two samples are available.
  obs_rows <- dat$EVID == 0
  dose_counts_tab <- table(dat$dosenr[obs_rows])
  dose_nr_candidates <- as.integer(names(dose_counts_tab)[dose_counts_tab >= 2])

  if(length(dose_nr_candidates) > 0) {
    dose_nr <- dose_nr_candidates[1]
  } else {
    ## take first observation for which at least one obs is available
    dose_nr_candidates <- as.integer(names(dose_counts_tab)[dose_counts_tab == 1])
    if(length(dose_nr_candidates) == 0) { # no observations in data
      return()
    }
    dose_nr <- dose_nr_candidates[1]
  }

  ## get peak value. This leads to estimate for V
  obs_mask <- dat$dosenr == dose_nr & dat$EVID == 0 & !is.na(dat$DV) & dat$DV != 0
  obs <- dat[obs_mask, ]
  tmp <- tail(obs, 3)
  dose <- dat$AMT[dat$dosenr == dose_nr & dat$EVID == 1]
  est <- c()
  n_points <- length(unique(tmp$TIME))
  if(inherits(tmp$TIME, "numeric") && n_points > 1) { # two datapoints at least
    fit <- stats::lm(log(DV) ~ TIME, tmp)
    KEL <- -as.numeric(coef(fit)[2])
    if(is.null(KEL) || is.na(KEL)) {
      est$V <- NA
      est$CL <- NA
    } else {
      if (KEL <= 0) { # fallback for absorption-phase or flat data
        KEL <- (log(max(obs$DV, na.rm=TRUE)) - log(min(obs$DV[obs$DV > 0], na.rm=TRUE))) / diff(range(obs$TIME))
      }
    }
    est$V <- dose / max(obs$DV, na.rm=TRUE)
    est$CL <- KEL * est$V
    est$weight <- n_points
  } else { # more crude estimation
    if(length(tmp$DV) > 0) {
      est$V <- dose / (max(tmp$DV, na.rm=TRUE) * 5)
      est$CL <- est$V / 20
      est$weight <- 0.001 # downweigh this, since very crude estimate. Only use if no subjects with >2 samples available
    } else { # for placebo patients, DV may all be zero or NA so we should not attempt to ballpark V or CL
      est$V <- NA
      est$CL <- NA
      est$weight <- 0
    }
  }

  unlist(est)
}
