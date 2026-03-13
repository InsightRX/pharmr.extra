#' Set the dependent variable (DV) column in a Pharmpy model's datainfo
#'
#' Updates the `datainfo` object so that the specified column has type `'dv'`.
#' Any column that previously had type `'dv'` is demoted to type `'unknown'`.
#'
#' @param model Pharmpy model object
#' @param dv Name of the column to set as the dependent variable
#'
#' @returns Pharmpy model object with updated datainfo
#'
#' @export
set_dv <- function(model, dv) {
  if (!is.character(dv) || length(dv) != 1L || is.na(dv)) {
    cli::cli_abort("`dv` must be a single character string.")
  }
  di <- model$datainfo
  col_names <- di$names
  if (!dv %in% col_names) {
    cli::cli_abort(
      "Column {.val {dv}} not found in datainfo. Available columns: {.val {col_names}}."
    )
  }
  # Demote the current DV column to 'unknown' if one exists
  tryCatch({
    current_dv_col <- di$dv_column
    if (current_dv_col$name == dv) return(model)
    new_old_col <- current_dv_col$replace(type = 'unknown')
    di <- di$set_column(new_old_col)
  }, error = function(e) NULL)
  # Promote the target column to 'dv'
  target_col <- di[[dv]]
  new_col <- target_col$replace(type = 'dv')
  di <- di$set_column(new_col)
  # Apply to model and update source
  new_model <- model$replace(datainfo = di)
  new_model$update_source()
  # Confirm the update took effect
  actual_dv <- tryCatch(new_model$datainfo$dv_column$name, error = function(e) NULL)
  if (!identical(actual_dv, dv)) {
    cli::cli_abort("Failed to set DV column to {.val {dv}}: datainfo was not updated as expected.")
  }
  new_model
}
