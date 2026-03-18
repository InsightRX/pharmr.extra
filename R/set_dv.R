#' Set the dependent variable (DV) column in a Pharmpy model's datainfo
#'
#' Updates the `$INPUT` record in the NONMEM model code so that the specified
#' column is mapped to NONMEM's internal `DV` variable, and updates the
#' `datainfo` accordingly. Any column that previously had type `'dv'` is
#' demoted to type `'unknown'`.
#'
#' @param model Pharmpy model object
#' @param dv Name of the column to set as the dependent variable
#'
#' @returns Pharmpy model object with updated datainfo and $INPUT record
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
  # Get current DV column name
  old_dv <- tryCatch(model$datainfo$dv_column$name, error = function(e) NULL)
  if (identical(old_dv, dv)) return(model)

  # Update the $INPUT record via string manipulation then re-read the model.
  # Re-reading is simpler and more reliable than manipulating pharmpy's internal
  # parse tree: pharmpy auto-derives the correct datainfo from the new $INPUT.
  model_name <- tryCatch(model$name, error = function(e) NULL)
  new_code <- .update_input_dv_in_code(model$code, old_dv = old_dv, new_dv = dv)
  new_model <- pharmr::read_model_from_string(new_code)
  if (!is.null(model_name)) {
    new_model <- pharmr::set_name(new_model, model_name)
  }

  # Confirm the update took effect
  actual_dv <- tryCatch(new_model$datainfo$dv_column$name, error = function(e) NULL)
  if (!identical(actual_dv, dv)) {
    cli::cli_abort("Failed to set DV column to {.val {dv}}: datainfo was not updated as expected.")
  }
  new_model
}

# Update the $INPUT record in NONMEM model code to switch the DV column.
#
# old_dv: dataset column name currently mapped to NONMEM's DV variable
# new_dv: dataset column name to map to NONMEM's DV variable
#
# Rules applied only to $INPUT record lines:
#  - old_dv == 'DV': standalone "DV" → "DROP"
#    ('DV' is reserved in NONMEM and cannot be used as an ordinary column)
#  - old_dv != 'DV': "DV=<old_dv>" → standalone "<old_dv>"
#  - new_dv != 'DV': standalone "<new_dv>" → "DV=<new_dv>"
#  - new_dv == 'DV': first "DROP" → "DV" (restores the original DV position)
.update_input_dv_in_code <- function(model_code, old_dv, new_dv) {
  lines <- strsplit(model_code, "\n", fixed = TRUE)[[1]]
  in_input <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    stripped <- trimws(line)

    # Detect record boundaries
    if (nchar(stripped) > 0 && substr(stripped, 1L, 1L) == "$") {
      in_input <- grepl(
        "^\\$IN(P(U(T?)?)?)?($|\\s)",
        stripped, ignore.case = TRUE, perl = TRUE
      )
    }

    if (!in_input) next

    # Demote old DV column
    if (!is.null(old_dv)) {
      if (toupper(old_dv) == "DV") {
        # Standalone "DV" (not followed by "=") → "DROP"
        line <- gsub("\\bDV\\b(?!=)", "DROP", line, perl = TRUE, ignore.case = TRUE)
      } else {
        # "DV=<old_dv>" → standalone "<old_dv>"
        line <- gsub(
          paste0("\\bDV=", .re_escape(old_dv), "\\b"),
          old_dv, line, perl = TRUE, ignore.case = TRUE
        )
      }
    }

    # Promote new DV column
    if (toupper(new_dv) != "DV") {
      # Standalone "<new_dv>" (not preceded by "=") → "DV=<new_dv>"
      line <- gsub(
        paste0("(?<![=A-Za-z0-9_])", .re_escape(new_dv), "\\b"),
        paste0("DV=", new_dv), line, perl = TRUE, ignore.case = TRUE
      )
    } else {
      # new_dv == "DV": restore first "DROP" → "DV"
      line <- sub("\\bDROP\\b", "DV", line, perl = TRUE, ignore.case = TRUE)
    }

    lines[[i]] <- line
  }

  paste(lines, collapse = "\n")
}

.re_escape <- function(x) gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)

# Set the type on a ColumnInfo, handling pharmpy API differences between v1 and v2.
# In pharmpy < 2.0, `type` is a direct attribute of ColumnInfo.
# In pharmpy >= 2.0, `type` lives inside the DataVariable (variable_mapping).
.col_with_type <- function(col, type) {
  tryCatch(
    col$replace(type = type),
    error = function(e) {
      vm <- col$variable_mapping
      col$replace(variable_mapping = vm$replace(type = type))
    }
  )
}
