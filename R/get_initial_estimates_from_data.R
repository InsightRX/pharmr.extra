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
#'
#' @export
get_initial_estimates_from_data <- function(
  data,
  n_cmt = 1,
  scale_observations = NULL
) {

  ## TODO: an extension could be to automatically add
  ## observed value scaling, e.g. when V < 1.0.
  pars <- data.frame()
  ids <- unique(data$ID)
  for(id in ids) {
    tmp <- get_initial_estimates_from_individual_data(
      data[data$ID == id,]
    )
    if(length(tmp) > 0) {
      pars <- dplyr::bind_rows(pars, tmp)
    }
  }
  est <- pars |>
    dplyr::summarise_all(function(x) signif(mean(x, na.rm=TRUE), 3)) |>
    as.list()
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
#' @param data TODO
#' @param ... TODO
get_initial_estimates_from_individual_data <- function(data, ...) {

  suppressWarnings(
    dat <- data |>
      dplyr::mutate(
        dosenr = cumsum(.data$EVID),
        DV = as.numeric(.data$DV),
        TIME = as.numeric(.data$TIME)
      )
  )

  ## Get first dose number for which more than two samples are available.
  dose_nr <- dat |>
    dplyr::filter(.data$EVID == 0) |>
    dplyr::group_by(.data$dosenr) |>
    dplyr::summarise(n_obs = length(.data$TIME)) |>
    dplyr::filter(.data$n_obs >= 2) |>
    dplyr::slice(1) |>
    dplyr::pull("dosenr")

  if(length(dose_nr) == 0) {
    ## take first observation for which at least one obs is available
    dose_nr <- dat |>
      dplyr::filter(.data$EVID == 0) |>
      dplyr::group_by(.data$dosenr) |>
      dplyr::summarise(n_obs = length(.data$TIME)) |>
      dplyr::filter(.data$n_obs == 1) |>
      dplyr::slice(1) |>
      dplyr::pull("dosenr")
  }
  if(length(dose_nr) == 0) { # no observations in data
    return()
  }

  ## get peak value. This leads to estimate for V
  tmp <- dat |>
    dplyr::filter(
      .data$dosenr == dose_nr & .data$EVID == 0 & !is.na(.data$DV) & .data$DV != 0
    ) |>
    dplyr::slice(unique(c(which.max(.data$DV), which.min(.data$DV))))
  dose <- dat |>
    dplyr::filter(.data$dosenr == dose_nr & .data$EVID == 1) |>
    dplyr::pull("AMT")
  est <- c()
  if(inherits(tmp$TIME, "numeric") && nrow(tmp) > 1) { # two datapoints at least
    KEL <- (log(max(tmp$DV)) - log(min(tmp$DV))) / abs(diff(tmp$TIME))
    est$V <- dose / max(tmp$DV, na.rm=TRUE)
    est$CL <- KEL * est$V
  } else { # more crude estimation
    if(length(tmp$DV) > 0) {
      est$V <- dose / (max(tmp$DV, na.rm=TRUE) * 5)
      est$CL <- est$V / 10
    } else { # for placebo patients, DV may all be zero or NA so we should not attempt to ballpark V or CL
      est$V <- NA
      est$CL <- NA
    }
  }

  unlist(est)
}
