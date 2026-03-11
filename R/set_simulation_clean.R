#' Set model to be a simulation (only) model
#' 
#' This function is a drop-in replacement of the Pharmpy `set_simulation()`
#' function. The pharmpy function works fine in many instances, but in some 
#' cases it modifies the variable declarations (e.g. redeclares variables in $PK
#' that are declared in $DES, which can lead to invalid models that are not 
#' accepted by NONMEM).
#' 
#' This function just removes the $ESTIMATION steps and adds the $SIM record,
#' and does not modify any of the other NONMEM code. 
#' 
#' @inheritParams run_nlme
#' @param seed random seed number
#' @param n number of simulation subproblems to run
#' 
#' @returns a Pharmpy NONMEM model object
#' 
#' @export
#' 
set_simulation_clean <- function(model, seed, n) {
  steps <- get_estimation_steps(model)
  for(key in steps) {
    model <- model |> 
      pharmr::remove_estimation_step(0)
  }
  model_code <- model$code
  model_code <- paste0(
    paste(stringr::str_replace_all(model$code, "\\t", " "), collapse = ""),
    glue::glue("\n$SIMULATION ({seed}) SUBPROBLEMS={n} ONLYSIMULATION\n")
  )
  sim_model <- pharmr::read_model_from_string(model_code)  
  sim_model
} 
