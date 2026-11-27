# Input row index of run_sim() output (`.row`, #164) ---------------------------

.row_map <- function(data) {
  sim_row_map(number_sim_rows(data))
}

test_that("sim_row_map keeps the row numbers and the numeric keys", {
  dat <- data.frame(ID = c(1, 1), TIME = c(0, 1), DV = 0, EVID = c(1, 0),
                    CMT = c("depot", "central"))
  map <- .row_map(dat)
  expect_equal(map$.row, 1:2)
  expect_equal(names(map), c(".row", "ID", "TIME", "EVID"))
  ## no row numbers, no map
  expect_null(sim_row_map(dat))
})

test_that("resolve_sim_regimens numbers the input rows before sorting", {
  dat <- data.frame(
    ID = c(2, 1, 1, 2, 1), TIME = c(0, 6, 0, 6, 0), DV = 0,
    AMT = c(100, 0, 0, 0, 100), EVID = c(1, 0, 0, 0, 1),
    .regimen = c("b", "a", "a", "b", "a")
  )
  regs <- resolve_sim_regimens(dat, input_data = NULL, verbose = FALSE)
  ## the row numbers refer to `dat` as passed, across regimens
  expect_equal(regs[[1]]$label, "b")
  expect_equal(regs[[1]]$row_map$.row, c(1L, 4L))
  ## regimen "a" sorted to ID, TIME, dose first: rows 5 (dose), 3, 2
  expect_equal(regs[[2]]$row_map$.row, c(5L, 3L, 2L))
  expect_equal(regs[[2]]$row_map$EVID, regs[[2]]$data$EVID)
  ## and the bookkeeping column does not go to NONMEM
  for(reg in regs) expect_false(sim_row_column %in% names(reg$data))
})

test_that("add_sim_row_index matches record for record despite rounding", {
  dat <- data.frame(ID = c(1, 1, 1), TIME = c(0, 1.8970833333, 1.8970833333),
                    EVID = c(1, 0, 0), CMT = c(1, 2, 3))
  map <- sim_row_map(transform(dat, .run_sim_input_row = c(7L, 3L, 9L)))
  ## TIME as NONMEM writes it, 5 significant digits
  tab <- data.frame(ID = 1, TIME = signif(dat$TIME, 5), EVID = dat$EVID,
                    CMT = dat$CMT, DV = 1:3)
  out <- add_sim_row_index(tab, map)
  expect_equal(out$.row, c(7L, 3L, 9L))
  ## a row that is not the record it should be: no index at all
  tab$CMT <- c(1, 3, 2)
  expect_warning(out <- add_sim_row_index(tab, map), "Could not match 3")
  expect_true(all(is.na(out$.row)))
  expect_silent(add_sim_row_index(tab, map, warn = FALSE))
})

test_that("add_sim_row_index matches each subproblem on its own", {
  dat <- data.frame(ID = c(1, 1, 2), TIME = c(0, 1, 0), EVID = 0)
  map <- .row_map(dat)
  tab <- rbind(dat, dat)
  ## told apart by the subproblem column ...
  tab$.subproblem <- rep(1:2, each = 3)
  expect_equal(add_sim_row_index(tab, map)$.row, rep(1:3, 2))
  ## ... or the table being a multiple of the input in length
  tab$.subproblem <- NULL
  expect_equal(add_sim_row_index(tab, map)$.row, rep(1:3, 2))
  ## and an NWPRI table is split on `.uncertainty`
  tab$.uncertainty <- rep(c(4L, 9L), each = 3)
  expect_equal(add_sim_row_index(tab, map)$.row, rep(1:3, 2))
})

test_that("add_sim_row_index matches the records a $DATA filter kept", {
  dat <- data.frame(ID = c(1, 1, 1, 2, 2), TIME = c(0, 1, 2, 0, 1),
                    EVID = c(1, 0, 0, 1, 0))
  map <- .row_map(dat)
  ## records 2 and 4 dropped
  tab <- dat[c(1, 3, 5), ]
  expect_equal(add_sim_row_index(tab, map)$.row, c(1L, 3L, 5L))
})

test_that("add_sim_row_index leaves an ambiguous match NA", {
  ## two observations with the same keys, one of which NONMEM dropped: which
  ## one is left cannot be told from the table
  dat <- data.frame(ID = 1, TIME = c(0, 1, 1, 2), EVID = c(1, 0, 0, 0))
  map <- .row_map(dat)
  tab <- dat[c(1, 2, 4), ]
  expect_warning(out <- add_sim_row_index(tab, map), "Could not match 1 of 3")
  expect_equal(out$.row, c(1L, NA, 4L))
})

test_that("add_sim_row_index needs ID and TIME to match a filtered table", {
  dat <- data.frame(ID = c(1, 1, 2), TIME = c(0, 1, 0))
  map <- .row_map(dat)
  ## FIRSTONLY-style table without TIME: nothing to match on
  tab <- data.frame(ID = c(1, 2), CL = 1)
  expect_equal(add_sim_row_index(tab, map, warn = FALSE)$.row,
               c(NA_integer_, NA_integer_))
  ## but a table with a row per record needs no keys at all
  tab <- data.frame(CL = 1:3)
  expect_equal(add_sim_row_index(tab, map)$.row, 1:3)
})

test_that("add_sim_row_index passes through without a map", {
  tab <- data.frame(ID = 1)
  expect_identical(add_sim_row_index(tab, NULL), tab)
  expect_equal(add_sim_row_index(tab[0, , drop = FALSE], .row_map(tab))$.row,
               integer(0))
})

test_that("run_sim_nlmixr: .row points at the input rows simulated", {
  skip_if_not_installed("rxode2")
  code <- paste0(
    "sim_model <- function() {\n",
    "  ini({\n    POP_CL <- 5\n    POP_V <- 50\n    eta.cl ~ 0.09\n",
    "    add_err <- 0.1\n  })\n",
    "  model({\n    CL <- POP_CL * exp(eta.cl)\n    V <- POP_V\n",
    "    d/dt(A_CENTRAL) <- -CL/V * A_CENTRAL\n",
    "    IPRED <- A_CENTRAL / V\n    IPRED ~ add(add_err)\n  })\n}\n"
  )
  ## unsorted, with a row-level column the output does not carry
  dat <- data.frame(
    ID = c(2, 1, 1, 2, 1, 2), TIME = c(1.8970833333, 0, 1.8970833333, 0, 4, 4),
    DV = 0, AMT = c(0, 100, 0, 200, 0, 0), EVID = c(0, 1, 0, 1, 0, 0),
    MDV = c(0, 1, 0, 1, 0, 0), CMT = 1, SAMPLE = c(11, 12, 13, 14, 15, 16)
  )
  out <- run_sim_nlmixr(data = dat, model_code = code, seed = 1,
                        n_iterations = 2, verbose = FALSE)
  ## doses have no output row; every observation shows up once per iteration
  expect_equal(nrow(out), 8)
  expect_false(anyNA(out$.row))
  expect_equal(sort(unique(out$.row)), c(1L, 3L, 5L, 6L))
  expect_equal(out$ID, dat$ID[out$.row])
  expect_equal(out$TIME, dat$TIME[out$.row])
  expect_equal(dat$SAMPLE[out$.row[out$ID == 1 & out$TIME == 4]], c(15, 15))
  ## and the bookkeeping column stays internal
  expect_false(sim_row_column %in% names(out))
})
