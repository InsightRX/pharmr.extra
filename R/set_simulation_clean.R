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
#' @param model Pharmpy NONMEM model object
#' @param seed random seed number
#' @param n number of simulation subproblems to run
#' 
#' @returns a Pharmpy NONMEM model object
#' 
#' @export
#' 
set_simulation_clean <- function(model, seed, n) {
  model <- model |> 
    remove_estimation_steps_from_model()
  model_code <- model$code
  model_code <- paste0(
    paste(stringr::str_replace_all(model$code, "\\t", " "), collapse = ""),
    glue::glue("\n\n$SIMULATION ({seed}) SUBPROBLEMS={n} ONLYSIMULATION\n")
  )
  sim_model <- pharmr::read_model_from_string(model_code)  
  sim_model
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
