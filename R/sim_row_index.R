## Input row index for run_sim() output (`.row`)
##
## run_sim() sorts every regimen's dataset (`ID`, `TIME`, `-EVID`) before it is
## simulated, and the NONMEM `$TABLE` it reads back holds only the columns it
## asked for, written at 5 significant digits. So a row-level column of the
## caller's dataset cannot be joined back onto the output reliably: an exact
## join on ID/TIME/EVID fails on rounded times and on repeated keys. Instead,
## every input row is numbered in R before the dataset is split and sorted, and
## that number is returned on the output rows as `.row` (#164).
##
## NONMEM, with the `$TABLE` run_sim() writes (`update_table = TRUE`): the row
## number is carried *through* the simulation. It goes into the simulation
## dataset as two data items appended to `$INPUT` and is written to the table,
## so every record NONMEM keeps comes back with its own row number whatever
## `$DATA` `IGNORE`/`ACCEPT` filters drop. Two items rather than one because a
## table column is written to 5 significant digits (NONMEM's default
## `s1PE11.4`, and `FORMAT` cannot be set for one column only): the row number
## is split as `SROWH * 10000 + SROWL`, both of which stay below 1e5 and so are
## written exactly, for datasets of up to a billion rows.
##
## Elsewhere (nlmixr2, which reports no data items; a table the model declares
## under `update_table = FALSE`) the output is matched to the input it was
## simulated from instead:
##
## - When the table has one row per input record, the match is by position.
##   The keys present in both (`ID`, `TIME`, `EVID`, `CMT`) are checked against
##   the input as a guard, allowing for the table's rounding.
## - When records are missing from the table, the table is an order-preserving
##   subsequence of the input, and its rows are matched on those keys. A row
##   whose match is ambiguous -- a dropped record with the same keys as a kept
##   one next to it -- gets `NA` rather than a guess.

## Column the input row number travels under inside run_sim(). Not `.row`, so
## that a caller's own `.row` column is left alone.
sim_row_column <- ".run_sim_input_row"

#' Number the rows of a simulation dataset
#'
#' @param data simulation dataset.
#'
#' @returns `data` with an integer `sim_row_column` holding the row numbers.
#' @noRd
number_sim_rows <- function(data) {
  data[[sim_row_column]] <- seq_len(nrow(data))
  data
}

#' What `add_sim_row_index()` matches output rows against
#'
#' @param data one regimen's dataset, in simulation order, carrying the
#' `sim_row_column`.
#' @param keys the columns to match on, where `data` has them.
#'
#' @returns a data.frame with `.row` (the input row number) and the `keys`
#' `data` has, as numbers (a key that is not numeric is left out). `NULL`
#' when `data` carries no row numbers.
#' @noRd
sim_row_map <- function(data, keys = c("ID", "TIME", "EVID", "CMT")) {
  if(is.null(data) || !sim_row_column %in% names(data)) return(NULL)
  map <- data.frame(.row = as.integer(data[[sim_row_column]]))
  for(key in intersect(keys, names(data))) {
    value <- suppressWarnings(as.numeric(as.character(data[[key]])))
    ## A key that does not read as a number (an nlmixr2 compartment name, say)
    ## cannot be compared to the table's; matching on fewer keys is fine.
    if(all(is.na(value) == is.na(data[[key]]))) map[[key]] <- value
  }
  map
}

#' Add the input row index (`.row`) to a simulation output table
#'
#' @param tab simulation output table for one regimen. A table that stacks
#' several subproblems (`n_iterations`, uncertainty draws) is matched per
#' subproblem, told apart by `.subproblem`, `.uncertainty` or `sim.id`, or
#' else by the table being an exact multiple of the input in length.
#' @param row_map the output of `sim_row_map()` for that regimen.
#' @param warn warn when some output rows could not be matched?
#'
#' @returns `tab` with an integer `.row` column (`NA` where no unambiguous
#' match was found), or `tab` unchanged when there is no `row_map`. A table
#' carrying the row number itself (`sim_row_items`, see
#' `add_sim_row_items()`) is read off instead, and those columns dropped.
#' @noRd
add_sim_row_index <- function(tab, row_map, warn = TRUE) {
  if(is.null(tab)) return(tab)
  ## Carried through NONMEM: read off the table, no matching needed.
  if(all(sim_row_items %in% names(tab))) {
    return(decode_sim_row_items(tab, row_map, warn = warn))
  }
  if(is.null(row_map) || nrow(row_map) == 0) return(tab)
  n <- nrow(tab)
  if(n == 0) {
    tab$.row <- integer(0)
    return(tab)
  }
  block_col <- intersect(c(".subproblem", ".uncertainty", "sim.id"), names(tab))
  blocks <- if(length(block_col) > 0) {
    b <- tab[[block_col[1]]]
    split(seq_len(n), factor(b, levels = unique(b)))
  } else if(n %% nrow(row_map) == 0) {
    split(seq_len(n), rep(seq_len(n / nrow(row_map)), each = nrow(row_map)))
  } else {
    list(seq_len(n))
  }
  idx <- rep(NA_integer_, n)
  for(rows in blocks) {
    idx[rows] <- match_sim_rows(tab[rows, , drop = FALSE], row_map)
  }
  if(warn && anyNA(idx)) {
    cli::cli_warn(c(
      "Could not match {sum(is.na(idx))} of {n} simulation output row{?s} to \\
       the input dataset; their {.field .row} is {.val {NA}}.",
      i = "The output table does not line up with the simulation dataset \\
           record for record (records dropped by {.code $DATA} filters that \\
           share {.field ID}/{.field TIME}/{.field EVID} with kept ones, or a \\
           table that is not written for every record)."
    ))
  }
  tab$.row <- idx
  tab
}

#' Match one subproblem's output rows to the input rows
#'
#' @param block output rows of one subproblem.
#' @param map the output of `sim_row_map()`.
#'
#' @returns integer vector of input row numbers, one per row of `block`, `NA`
#' where there is no unambiguous match.
#' @noRd
match_sim_rows <- function(block, map) {
  n <- nrow(block)
  m <- nrow(map)
  none <- rep(NA_integer_, n)
  keys <- intersect(setdiff(names(map), ".row"), names(block))
  out_keys <- lapply(keys, function(k) {
    suppressWarnings(as.numeric(as.character(block[[k]])))
  })
  in_keys <- lapply(keys, function(k) map[[k]])

  ## NONMEM writes table values to 5 significant digits by default, so allow
  ## for that rounding (relative error up to 5e-5) and no more.
  close <- function(a, b) {
    !is.na(a) & !is.na(b) & abs(a - b) <= 1e-4 * pmax(abs(a), abs(b)) + 1e-10
  }

  if(n == m) {
    ok <- rep(TRUE, n)
    for(k in seq_along(keys)) ok <- ok & close(out_keys[[k]], in_keys[[k]])
    if(all(ok)) return(map$.row)
  }

  ## Not record for record: the output must then be a subsequence of the
  ## input, which can only be matched on keys that identify a record.
  if(n >= m || !all(c("ID", "TIME") %in% keys)) return(none)

  same <- function(i, j) {
    for(k in seq_along(keys)) {
      if(!close(out_keys[[k]][i], in_keys[[k]][j])) return(FALSE)
    }
    TRUE
  }
  ## Earliest and latest embedding of the output in the input. Every possible
  ## matching lies between the two, so a row both put on the same input row
  ## has exactly one match; anywhere they differ, the match is ambiguous.
  first <- integer(n)
  j <- 1L
  for(i in seq_len(n)) {
    while(j <= m && !same(i, j)) j <- j + 1L
    if(j > m) return(none)
    first[i] <- j
    j <- j + 1L
  }
  last <- integer(n)
  j <- m
  for(i in rev(seq_len(n))) {
    while(j >= 1L && !same(i, j)) j <- j - 1L
    if(j < 1L) return(none)
    last[i] <- j
    j <- j - 1L
  }
  out <- map$.row[first]
  out[first != last] <- NA_integer_
  out
}

# Carrying the row number through NONMEM ---------------------------------------

## Data items the row number travels through NONMEM in (high and low part).
sim_row_items <- c("SROWH", "SROWL")
sim_row_base <- 10000

#' The items of a control stream's `$INPUT` record(s)
#'
#' @param code NONMEM control stream (a single string).
#'
#' @returns character vector, one element per data item (`DV=CONC` and
#' `X=DROP` count as one), or `character(0)` without an `$INPUT` record.
#' @noRd
nm_input_items <- function(code) {
  lines <- strsplit(code, "\n", fixed = FALSE)[[1]]
  in_input <- FALSE
  text <- character(0)
  for(line in lines) {
    stripped <- trimws(sub(";.*$", "", line))
    if(startsWith(stripped, "$")) {
      in_input <- grepl("^\\$IN(P(U(T)?)?)?($|\\s)", stripped,
                        ignore.case = TRUE)
      if(in_input) stripped <- sub("^\\$\\S+", "", stripped)
    }
    if(in_input) text <- c(text, stripped)
  }
  text <- gsub("\\s*=\\s*", "=", paste(text, collapse = " "))
  items <- strsplit(trimws(text), "\\s+")[[1]]
  items[nzchar(items)]
}

#' Carry the input row number through a simulation model
#'
#' Appends `sim_row_items` to the `$INPUT` record and to the simulation
#' `$TABLE` run_sim() wrote, so NONMEM reads every record's row number from
#' the dataset and writes it back next to that record's output. The dataset
#' has to carry the items at the matching position: see
#' `add_sim_row_data()`.
#'
#' Left alone when the model already uses either name, or has no `$INPUT`.
#'
#' @param sim_model Pharmpy NONMEM model, as `build_nonmem_sim_model()` builds
#' it, with a single `$TABLE` writing `output_file`.
#' @param output_file the simulation table's file name.
#'
#' @returns the updated model (with no dataset attached, like
#' `build_nonmem_sim_model()`'s), or `sim_model` unchanged.
#' @noRd
add_sim_row_items <- function(sim_model, output_file) {
  code <- sim_model$code
  if(length(nm_input_items(code)) == 0) return(sim_model)
  in_use <- vapply(sim_row_items, function(item) {
    grepl(paste0("\\b", item, "\\b"), code, ignore.case = TRUE)
  }, logical(1))
  if(any(in_use)) return(sim_model)

  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  is_record <- startsWith(trimws(lines), "$")
  input_start <- which(is_record & grepl(
    "^\\s*\\$IN(P(U(T)?)?)?($|\\s)", lines, ignore.case = TRUE
  ))
  table_start <- which(is_record & grepl(
    "^\\s*\\$TAB(L(E)?)?($|\\s)", lines, ignore.case = TRUE
  ))
  record_end <- function(start) {
    nxt <- which(is_record & seq_along(lines) > start)
    if(length(nxt) == 0) length(lines) else nxt[1] - 1
  }
  ## The table run_sim() wrote: the one whose FILE is `output_file`.
  table_start <- Filter(function(start) {
    rec <- paste(lines[start:record_end(start)], collapse = " ")
    grepl(paste0("FILE\\s*=\\s*", output_file, "(\\s|$)"), rec)
  }, table_start)
  if(length(input_start) == 0 || length(table_start) == 0) return(sim_model)

  items <- paste0("  ", paste(sim_row_items, collapse = " "))
  ## Insert after the last line of the (last) $INPUT record and right after
  ## the $TABLE line, back to front so the line numbers stay valid.
  inserts <- c(record_end(max(input_start)), table_start[1])
  for(at in sort(inserts, decreasing = TRUE)) {
    lines <- append(lines, items, after = at)
  }
  new_model <- pharmr::read_model_from_string(
    code = paste(lines, collapse = "\n")
  )
  new_model$replace(dataset = reticulate::py_none())
}

#' Add the row number data items to a regimen's simulation dataset
#'
#' Puts `sim_row_items` where NONMEM reads them: right after the columns the
#' rest of `$INPUT` covers (columns of the dataset beyond those are never read
#' by NONMEM and move along to the right). A dataset with fewer columns than
#' `$INPUT` items is padded with zeros, which is what NONMEM reads for the
#' missing trailing fields anyway.
#'
#' @param data one regimen's simulation dataset, in simulation order.
#' @param rows the input row number of every record of `data`.
#' @param code the simulation model's control stream.
#'
#' @returns `data` with the items added, or unchanged when the model does not
#' carry them (see `add_sim_row_items()`).
#' @noRd
add_sim_row_data <- function(data, rows, code) {
  items <- toupper(nm_input_items(code))
  n <- length(items)
  if(n < 2 || !identical(items[(n - 1):n], sim_row_items) || is.null(rows)) {
    return(data)
  }
  pos <- n - 2
  data <- as.data.frame(data)
  k <- 0
  while(ncol(data) < pos) {
    k <- k + 1
    data[[paste0(".pad", k)]] <- 0
  }
  rows <- as.integer(rows)
  carry <- data.frame(rows %/% sim_row_base, rows %% sim_row_base)
  names(carry) <- sim_row_items
  out <- cbind(data[, seq_len(pos), drop = FALSE], carry,
               data[, setdiff(seq_len(ncol(data)), seq_len(pos)), drop = FALSE])
  names(out) <- make.unique(names(out))
  out
}

#' Carry the row number through a simulation model's regimen datasets
#'
#' @param regimens the output of `resolve_sim_regimens()`.
#' @param sim_model the simulation model (see `add_sim_row_items()`).
#'
#' @returns `regimens`, every `data` with `sim_row_items` added where the
#' model reads them.
#' @noRd
add_sim_row_data_to_regimens <- function(regimens, sim_model) {
  code <- sim_model$code
  lapply(regimens, function(reg) {
    reg$data <- add_sim_row_data(reg$data, reg$row_map$.row, code)
    reg
  })
}

#' Read the row number NONMEM carried back off a simulation table
#'
#' @param tab simulation output table with `sim_row_items`.
#' @param row_map the output of `sim_row_map()`, used to check the decoded
#' numbers belong to this regimen's records.
#' @param warn warn about rows without a valid row number?
#'
#' @returns `tab` with `.row` and without `sim_row_items`.
#' @noRd
decode_sim_row_items <- function(tab, row_map, warn = TRUE) {
  high <- suppressWarnings(as.numeric(tab[[sim_row_items[1]]]))
  low <- suppressWarnings(as.numeric(tab[[sim_row_items[2]]]))
  row <- as.integer(round(high) * sim_row_base + round(low))
  if(!is.null(row_map)) row[!row %in% row_map$.row] <- NA_integer_
  if(warn && anyNA(row)) {
    cli::cli_warn(
      "{sum(is.na(row))} of {length(row)} simulation output row{?s} came \\
       back without a valid input row number; their {.field .row} is \\
       {.val {NA}}."
    )
  }
  tab[sim_row_items] <- NULL
  tab$.row <- row
  tab
}
