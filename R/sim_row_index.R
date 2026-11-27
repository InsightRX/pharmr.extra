## Input row index for run_sim() output (`.row`)
##
## run_sim() sorts every regimen's dataset (`ID`, `TIME`, `-EVID`) before it is
## simulated, and the NONMEM `$TABLE` it reads back holds only the columns it
## asked for, written at 5 significant digits. So a row-level column of the
## caller's dataset cannot be joined back onto the output reliably: an exact
## join on ID/TIME/EVID fails on rounded times and on repeated keys. Instead,
## every input row is numbered in R before the dataset is split and sorted, and
## that number is carried over to the output rows as `.row` (#164).
##
## The output is matched to the input it was simulated from, never rounded:
##
## - When the table has one row per input record (the usual case: NONMEM
##   writes one row per data record it reads), the match is by position. The
##   keys present in both (`ID`, `TIME`, `EVID`, `CMT`) are checked against
##   the input as a guard, allowing for the table's rounding.
## - When records are missing from the table (a `$DATA` `IGNORE`/`ACCEPT`
##   filter, say), the table is an order-preserving subsequence of the input,
##   and its rows are matched on those keys. A row whose match is ambiguous --
##   a dropped record with the same keys as a kept one next to it -- gets `NA`
##   rather than a guess.

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
#' match was found), or `tab` unchanged when there is no `row_map`.
#' @noRd
add_sim_row_index <- function(tab, row_map, warn = TRUE) {
  if(is.null(tab) || is.null(row_map) || nrow(row_map) == 0) return(tab)
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
