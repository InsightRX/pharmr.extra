#' Run simulation from a fitted object
#' 
#' Using fit object from fit with nlmixr2.
#' 
#' @param obj fit object from nlmixr2
#' @param dose vector of dose amounts. Needs to match length of `interval`
#' @param interval vector of dosing intervals. Needs to match length of `dose`
#' @param n_doses number of doses to simulate
#' @param n_days alternative to `n_doses`, specify number of days. `n_days`
#' takes precedence if both are specified.
#' @param n_subjects number of subjects to simulate
#' @param aggregate summarize the data using mean, median, sd, etc.
#' @param group grouping to be added to aggregated data? 
#' @param bsv Simulate using between subject variability
#' @param res_error Add residual unexplained error to the simulated data?
#' @param path path to file to store output object from fit.
#' @param ... arguments passed on to rxode2::rxSolve() function
#' 
#' @export
#' 
run_simulation <- function(
    obj,
    dose,
    interval,
    n_doses = NULL,
    n_days = 5,
    n_subjects = 500,
    aggregate = TRUE,
    bsv = TRUE,
    res_error = TRUE,
    group = NULL,
    path = NULL,
    ...
) {
  if(length(dose) != length(interval)) {
    stop("Length of `dose` vector needs to match length of `interval` vector.")
  }
  if(!bsv) {
    omega <- obj$omega * 1e-6 # cannot be NULL or matrix of 0s
  } else {
    omega <- obj$omega
  }
  if(is.null(n_doses) && is.null(n_days)) {
    stop("Either `n_doses` or `n_days` need to be specified.")
  }

  dat <- data.frame()
  scenarios <- 1:length(dose)
  for(i in scenarios) {
    if(!is.null(n_days)) {
      n_doses <- ceiling(n_days * 24/interval[i])
    }
    t_obs <- seq(0, (n_doses+1) * interval[i], by = 2)
    ev <- rxode2::eventTable() %>%
      rxode2::add.dosing(
        dose = dose[i],
        nbr.doses = n_doses, 
        dosing.interval = interval[i]
      ) %>%
      rxode2::add.sampling(t_obs)
    dat <- dplyr::bind_rows(
      dat,
      rxode2::rxSolve(
        object = obj,
        omega = omega,
        events = ev,
        nsim =  n_subjects,
        seed = 12345,
        returnType = "data.frame",
        simVariability = FALSE, # This seems to be "uncertainty in Theta" rather than "variability". We usually want this switched off.
        ...
      ) %>%
        dplyr::rename(
          y = "ipredSim",
          id = "sim.id"
        ) %>%
        dplyr::mutate(
          scenario = paste0("Scenario ", i, ": ", dose[i], " mg / ", interval[i], " hrs"), 
          dose = dose[i], 
          interval = interval[i]
        )
    )
  }

  ## add residual error
  prop_sd <- 0
  add_sd <- 0
  pars <- as.list(obj$fixef)
  if("prop_sd" %in% names(pars)) {
    prop_sd <- pars$prop_sd
  }
  if("add_sd" %in% names(pars)) {
    add_sd <- pars$add_sd
  }
  if(res_error) {
    dat$y <- PKPDsim::add_ruv(dat$y, list(add = add_sd, prop = prop_sd))
  }
  if(!is.null(group)) {
    dat <- dat %>%
      dplyr::group_by(tidyselect::all_of(group))
  }
  if(aggregate) {
    dat <- dat %>%
      dplyr::group_by(.data$time, .data$scenario, .add = TRUE) %>%
      dplyr::summarise(
        dose = dose[1],
        interval = interval[1],
        mean = mean(.data$y),
        median = stats::median(.data$y),
        q_5 = stats::quantile(.data$y, .05),
        q_95 = stats::quantile(.data$y, .95),
        sd = stats::sd(.data$y)
      )
  }
  
  ## save fit object to file
  if(!is.null(path)) {
    saveRDS(dat, path)
  }
  
  return(dat)
}
