#' Set model to be a simulation (only) model
#'
#' This function is a drop-in replacement of the Pharmpy `set_simulation()`
#' function. The Pharmpy function works fine in many instances, but in some
#' cases it modifies the variable declarations (e.g. redeclares variables in $PK
#' that are declared in $DES, which can lead to invalid models that are not
#' accepted by NONMEM).
#'
#' This function just removes the $ESTIMATION steps and adds the $SIM record,
#' and does not modify any of the other NONMEM code.
#'
#' @param model Pharmpy NONMEM model object
#' @param seed random seed number
#' @param n number of simulation subproblems to run
#' @param true_prior emit `TRUE=PRIOR`, i.e. have NONMEM draw a new parameter
#' vector from the model's `$PRIOR` record for every subproblem (see
#' [add_nwpri_prior()]). Default `FALSE`.
#'
#' @returns a Pharmpy NONMEM model object when `true_prior = FALSE`.
#'
#' When `true_prior = TRUE` the return value is instead the NONMEM model
#' **code** (a single string). Pharmpy's `$SIMULATION` grammar does not accept
#' the `TRUE=PRIOR` option and refuses to parse such a control stream, so the
#' record can only be carried at the code level; the caller is expected to
#' write the string out itself rather than round-trip it through Pharmpy.
#'
#' @export
#'
set_simulation_clean <- function(model, seed, n, true_prior = FALSE) {
  model <- model |>
    remove_estimation_steps_from_model()
  model_code <- paste(
    stringr::str_replace_all(model$code, "\\t", " "),
    collapse = ""
  )
  model_code <- set_simulation_record(
    code = model_code,
    seed = seed,
    n = n,
    true_prior = true_prior
  )
  if(isTRUE(true_prior)) {
    return(model_code)
  }
  pharmr::read_model_from_string(model_code)
}

#' Set the `$SIMULATION` record of a NONMEM control stream
#'
#' Works on the model code rather than on a Pharmpy model object, because
#' Pharmpy cannot parse `TRUE=PRIOR` (see [set_simulation_clean()]) and because
#' the `uncertainty_engine = "nwpri"` path of [run_sim()] only varies the seed
#' and the subproblem count between chunks — a string edit on an otherwise
#' finished control stream.
#'
#' An existing `$SIMULATION` record is replaced in place, so its position
#' relative to the `$TABLE` records is preserved. When the model has none, the
#' record is appended.
#'
#' @param code NONMEM model code (single string or character vector of lines).
#' @inheritParams set_simulation_clean
#'
#' @returns NONMEM model code as a single string.
#'
#' @export
set_simulation_record <- function(code, seed, n, true_prior = FALSE) {
  record <- glue::glue(
    "$SIMULATION ({seed}) SUBPROBLEMS={n}",
    if(isTRUE(true_prior)) " TRUE=PRIOR" else "",
    " ONLYSIMULATION"
  )
  lines <- unlist(stringr::str_split(code, "\n"))
  starts <- grep("^\\s*\\$", lines)
  if(length(starts) == 0) {
    cli::cli_abort("No NONMEM records found in the model code.")
  }
  ## `$SIM` is the shortest abbreviation NM-TRAN accepts for $SIMULATION.
  is_sim <- grepl("^\\s*\\$SIM", lines[starts], ignore.case = TRUE)
  if(!any(is_sim)) {
    return(paste(c(trimws(lines, which = "right"), "", as.character(record)),
                 collapse = "\n"))
  }
  ## Replace whole record blocks, not just their first line: a $SIMULATION
  ## record may continue onto following lines.
  bounds <- c(starts, length(lines) + 1L)
  keep <- rep(TRUE, length(lines))
  first <- NA_integer_
  for(k in which(is_sim)) {
    span <- seq.int(bounds[k], bounds[k + 1L] - 1L)
    keep[span] <- FALSE
    if(is.na(first)) first <- bounds[k]
  }
  lines[first] <- as.character(record)
  keep[first] <- TRUE
  paste(lines[keep], collapse = "\n")
}

#' Remove all estimation steps from Pharmpy model object
#'
#' @inheritParams set_simulation_clean
#'
remove_estimation_steps_from_model <- function(model) {

  tool <- get_tool_from_model(model)
  if(tool == "nonmem") {

    ## if there's no estimation steps to begin with, then just return unchanged
    steps <- get_estimation_steps(model)
    if(length(steps) == 0) {
      return(model)
    }

    ## workaround for dataset needed to circumvent issues re-reading the model file
    data <- model$dataset
    if(!is.null(data)) {
      temp_csv <- tempfile(fileext = ".csv")
      write.csv(data, temp_csv, quote=F, row.names=F)
      model <- pharmr::set_dataset(model, temp_csv)
    }
    code_without_est <- model$code |>
      remove_nonmem_records("EST") |>
      remove_nonmem_records("COV")
    model <- pharmr::read_model_from_string(
      code = code_without_est
    )

  } else {
    cli::cli_warn("Removing $ESTIMATION steps can only be done for NONMEM models")
  }

  model

}
