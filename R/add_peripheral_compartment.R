#' Add a peripheral compartment
#'
#' Wrapper around [pharmr::add_peripheral_compartment()] that also updates
#' `$TABLE` references to match the new ADVAN/TRANS parameterization.
#'
#' Pharmpy's `add_peripheral_compartment()` renames `V` to `V2` in `$PK`
#' (required for ADVAN4 TRANS4) but does **not** update `$TABLE` references.
#' This causes NONMEM NM-TRAN error 479 when the model already has a `patab`
#' table listing `V`. This wrapper detects when a `patab` table exists and
#' automatically refreshes it after adding the compartment.
#'
#' @inheritParams run_nlme
#' @param ... passed to [pharmr::add_peripheral_compartment()]
#'
#' @returns Updated Pharmpy model object
#'
#' @export
add_peripheral_compartment <- function(model, ...) {
  model <- pharmr::add_peripheral_compartment(model, ...)
  if ("patab" %in% get_tables_in_model_code(model$code)) {
    model <- update_pk_tables(model)
  }
  model
}
