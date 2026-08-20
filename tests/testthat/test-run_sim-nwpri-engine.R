# NWPRI uncertainty engine: chunking, seeds and collection (#130) -------------
#
# Everything here is pure R or runs against a mocked-out `call_nmfe()`, so it
# needs neither NONMEM nor pharmpy. The record construction the engine feeds on
# is covered in `test-add_nwpri_prior.R`.

# Chunking ---------------------------------------------------------------------

test_that("nwpri_chunk_sizes splits the draws as evenly as it can", {
  expect_equal(nwpri_chunk_sizes(100, 4), c(25L, 25L, 25L, 25L))
  expect_equal(nwpri_chunk_sizes(10, 3), c(4L, 3L, 3L))
  expect_equal(nwpri_chunk_sizes(7, 1), 7L)
  expect_type(nwpri_chunk_sizes(7, 2), "integer")

  ## Whatever the split, every draw is accounted for exactly once and no chunk
  ## is empty (NONMEM has no SUBPROBLEMS=0).
  for (n in c(1, 2, 5, 13, 500)) {
    for (k in c(1, 2, 3, 8, 64)) {
      sizes <- nwpri_chunk_sizes(n, k)
      expect_equal(sum(sizes), n)
      expect_true(all(sizes >= 1L))
      expect_lte(max(sizes) - min(sizes), 1L)
    }
  }
})

test_that("nwpri_chunk_sizes never asks for more chunks than draws", {
  expect_equal(nwpri_chunk_sizes(3, 8), c(1L, 1L, 1L))
  expect_length(nwpri_chunk_sizes(1, 16), 1L)
})

test_that("nwpri_chunk_sizes validates its input", {
  expect_error(nwpri_chunk_sizes(0, 2), "positive integer")
  expect_error(nwpri_chunk_sizes(-1, 2), "positive integer")
  expect_error(nwpri_chunk_sizes(10, 0), "`n_chunks` must be a positive integer")
})

# Seeds ------------------------------------------------------------------------

test_that("nwpri_chunk_seeds spaces the chunk seeds widely and keeps them distinct", {
  seeds <- nwpri_chunk_seeds(12345, 8)
  expect_length(seeds, 8)
  expect_type(seeds, "integer")
  expect_equal(seeds[1], 12345L)
  expect_equal(anyDuplicated(seeds), 0L)
  ## Separate seeds are not formally independent streams, so the gap between
  ## them is the only protection against overlapping draw sequences.
  expect_true(all(diff(seeds) >= 1e6))
  ## and stays inside NONMEM's seed range
  expect_true(all(seeds >= 1 & seeds < 2147483647))
})

test_that("nwpri_chunk_seeds is deterministic and stays in range when it wraps", {
  expect_identical(nwpri_chunk_seeds(1, 5), nwpri_chunk_seeds(1, 5))
  seeds <- nwpri_chunk_seeds(2147483000, 4)
  expect_true(all(seeds >= 1 & seeds < 2147483647))
  expect_equal(anyDuplicated(seeds), 0L)
})

test_that("nwpri_chunk_seeds validates the base seed", {
  expect_error(nwpri_chunk_seeds(-1, 2), "non-negative integer")
  expect_error(nwpri_chunk_seeds(1.5, 2), "non-negative integer")
  expect_error(nwpri_chunk_seeds("x", 2), "non-negative integer")
})

test_that("resolve_n_chunks defaults to n_cores and validates otherwise", {
  expect_equal(resolve_n_chunks(NULL, 8), 8L)
  expect_equal(resolve_n_chunks(3, 8), 3L)
  expect_type(resolve_n_chunks(3, 8), "integer")
  expect_error(resolve_n_chunks(0, 8), "positive integer")
  expect_error(resolve_n_chunks(2.5, 8), "positive integer")
  expect_error(resolve_n_chunks(c(1, 2), 8), "positive integer")
})

# Collection -------------------------------------------------------------------

## One chunk's worth of simulation output: `rows` rows per subproblem.
.chunk_table <- function(size, rows = 2, value = 1) {
  data.frame(
    ID          = rep(seq_len(rows), times = size),
    DV          = value + seq_len(rows * size) / 100,
    .subproblem = rep(seq_len(size), each = rows)
  )
}

.chunk_spec <- function(index, size, offset) {
  list(index = index, size = size, offset = offset, folder = tempdir())
}

test_that("collect_nwpri_chunks renumbers the subproblems into a global index", {
  specs <- list(.chunk_spec(1, 3, 0), .chunk_spec(2, 2, 3))
  chunks <- list(
    list(index = 1, result = .chunk_table(3), warnings = list()),
    list(index = 2, result = .chunk_table(2), warnings = list())
  )
  out <- collect_nwpri_chunks(chunks, specs, 5)

  ## Chunk-local 1..3 and 1..2 become a single 1..5 running over both.
  expect_equal(sort(unique(out$.uncertainty)), 1:5)
  expect_false(".subproblem" %in% names(out))
  expect_equal(nrow(out), 10)
  expect_equal(attr(out, "n_uncertainty_requested"), 5)
  expect_equal(attr(out, "n_uncertainty_kept"), 5)
  ## rows stay in chunk order, so the offsets are applied to the right rows
  expect_equal(out$.uncertainty, c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
})

test_that("collect_nwpri_chunks drops a failed chunk instead of the whole run", {
  specs <- list(.chunk_spec(1, 2, 0), .chunk_spec(2, 2, 2), .chunk_spec(3, 2, 4))
  chunks <- list(
    list(index = 1, result = .chunk_table(2), warnings = list()),
    list(index = 2, result = simpleError("nmfe died"), warnings = list()),
    list(index = 3, result = .chunk_table(2), warnings = list())
  )
  expect_warning(
    expect_warning(
      out <- collect_nwpri_chunks(chunks, specs, 6),
      "chunk 2 \\(2 draws\\) failed"
    ),
    "Only 4 of 6"
  )
  ## The surviving chunks keep the index they would have had, so a dropped
  ## chunk leaves a gap rather than silently renumbering the draws.
  expect_equal(sort(unique(out$.uncertainty)), c(1, 2, 5, 6))
  expect_equal(attr(out, "n_uncertainty_kept"), 4)
  expect_equal(attr(out, "n_uncertainty_requested"), 6)
})

test_that("collect_nwpri_chunks reports an empty chunk", {
  specs <- list(.chunk_spec(1, 2, 0), .chunk_spec(2, 2, 2))
  chunks <- list(
    list(index = 1, result = .chunk_table(2), warnings = list()),
    list(index = 2, result = .chunk_table(2)[0, ], warnings = list())
  )
  expect_warning(
    expect_warning(collect_nwpri_chunks(chunks, specs, 4), "produced no"),
    "Only 2 of 4"
  )
})

test_that("collect_nwpri_chunks reports a short chunk", {
  specs <- list(.chunk_spec(1, 5, 0))
  chunks <- list(list(index = 1, result = .chunk_table(3), warnings = list()))
  expect_warning(
    expect_warning(
      out <- collect_nwpri_chunks(chunks, specs, 5),
      "returned 3 subproblems of the 5 requested"
    ),
    "Only 3 of 5"
  )
  expect_equal(attr(out, "n_uncertainty_kept"), 3)
})

test_that("collect_nwpri_chunks aborts when every chunk failed", {
  specs <- list(.chunk_spec(1, 2, 0), .chunk_spec(2, 2, 2))
  chunks <- list(
    list(index = 1, result = simpleError("boom"), warnings = list()),
    list(index = 2, result = simpleError("boom"), warnings = list())
  )
  expect_error(
    suppressWarnings(collect_nwpri_chunks(chunks, specs, 4)),
    "All 2 NWPRI chunks failed"
  )
})

test_that("collect_nwpri_chunks re-emits warnings a chunk raised, labelled as chunks", {
  specs <- list(.chunk_spec(1, 2, 0))
  captured <- run_captured(1, function() {
    warning("something odd in the worker")
    .chunk_table(2)
  })
  expect_warning(
    collect_nwpri_chunks(list(captured), specs, 2),
    "Uncertainty chunk 1: something odd in the worker"
  )
})

# The chunked run, with NONMEM mocked out --------------------------------------

## Stand in for a NONMEM run: record what it was asked to run, and hand back a
## table shaped the way `read_table_nm(subproblems = TRUE)` would.
.fake_chunk_fn <- function(log_env) {
  function(spec) {
    run_captured(spec$index, function() {
      log_env$mods[[as.character(spec$index)]] <-
        readLines(file.path(spec$folder, "run.mod"))
      .chunk_table(spec$size, rows = 2, value = spec$index)
    })
  }
}

test_that("run_nwpri_regimen chunks the draws over its own run folders", {
  folder <- withr::local_tempdir()
  log_env <- new.env(parent = emptyenv())
  log_env$mods <- list()

  code <- paste("$PROBLEM t", "$DATA data.csv IGNORE=@", "$THETA (0,1)",
                "$SIMULATION (1) SUBPROBLEMS=1 ONLYSIMULATION",
                "$TABLE ID DV FILE=simtab", sep = "\n")

  mockery::stub(run_nwpri_regimen, "make_nwpri_chunk_fn",
                function(nmfe, output_file) .fake_chunk_fn(log_env))
  out <- run_nwpri_regimen(
    sim_code = code, n_uncertainty = 10, n_chunks = 4, seed = 500,
    folder = folder, output_file = "simtab", nmfe = "fake-nmfe",
    n_cores = 1, force = FALSE, verbose = FALSE
  )

  ## One run folder per chunk, under the regimen folder.
  expect_setdiff_empty <- function(x, y) expect_equal(setdiff(x, y), character(0))
  expect_setdiff_empty(paste0("uncertainty_chunk_", 1:4), list.dirs(folder, full.names = FALSE))

  ## `.uncertainty` runs 1..n over the chunks, in order.
  expect_equal(sort(unique(out$.uncertainty)), 1:10)
  expect_equal(nrow(out), 20)
  expect_equal(attr(out, "n_uncertainty_kept"), 10)

  ## Each chunk got its own seed and its share of the subproblems, and nothing
  ## else about the control stream changed.
  sizes <- nwpri_chunk_sizes(10, 4)
  seeds <- nwpri_chunk_seeds(500, 4)
  for (k in 1:4) {
    mod <- log_env$mods[[as.character(k)]]
    sim <- grep("^\\$SIM", mod, value = TRUE)
    expect_length(sim, 1)
    expect_equal(
      sim,
      sprintf("$SIMULATION (%d) SUBPROBLEMS=%d TRUE=PRIOR ONLYSIMULATION",
              seeds[k], sizes[k])
    )
    expect_true("$TABLE ID DV FILE=simtab" %in% mod)
    expect_true("$THETA (0,1)" %in% mod)
  }
})

test_that("run_nwpri_regimen honours n_chunks rather than the draw count", {
  folder <- withr::local_tempdir()
  log_env <- new.env(parent = emptyenv())
  log_env$mods <- list()
  code <- "$PROBLEM t\n$THETA (0,1)\n$SIMULATION (1) SUBPROBLEMS=1 ONLYSIMULATION"

  mockery::stub(run_nwpri_regimen, "make_nwpri_chunk_fn",
                function(nmfe, output_file) .fake_chunk_fn(log_env))
  out <- run_nwpri_regimen(
    sim_code = code, n_uncertainty = 6, n_chunks = 1, seed = 1,
    folder = folder, output_file = "simtab", nmfe = "fake-nmfe",
    n_cores = 1, force = FALSE, verbose = FALSE
  )
  ## One job, all six subproblems in it. This is the setting that makes a run
  ## reproducible independent of the machine's core count.
  expect_length(log_env$mods, 1)
  expect_match(grep("^\\$SIM", log_env$mods[["1"]], value = TRUE),
               "SUBPROBLEMS=6")
  expect_equal(sort(unique(out$.uncertainty)), 1:6)
})

test_that("run_nwpri_regimen refuses to reuse an existing chunk folder", {
  folder <- withr::local_tempdir()
  dir.create(file.path(folder, "uncertainty_chunk_1"))
  log_env <- new.env(parent = emptyenv()); log_env$mods <- list()
  mockery::stub(run_nwpri_regimen, "make_nwpri_chunk_fn",
                function(nmfe, output_file) .fake_chunk_fn(log_env))
  expect_error(
    run_nwpri_regimen(
      sim_code = "$PROBLEM t\n$THETA (0,1)", n_uncertainty = 2, n_chunks = 1,
      seed = 1, folder = folder, output_file = "simtab", nmfe = "fake",
      n_cores = 1, force = FALSE, verbose = FALSE
    ),
    "exists"
  )
})

test_that("a chunk whose NONMEM run wrote no table is reported, not returned empty", {
  folder <- withr::local_tempdir()
  fn <- make_nwpri_chunk_fn(nmfe = "fake-nmfe", output_file = "simtab")
  ## `call_nmfe()` returning quietly without writing a table is exactly the
  ## silent failure mode this guards against.
  mockery::stub(fn, "call_nmfe", function(...) invisible(NULL))
  chunk_folder <- file.path(folder, "uncertainty_chunk_1")
  dir.create(chunk_folder)
  res <- fn(list(index = 1, folder = chunk_folder, size = 2, offset = 0))
  expect_s3_class(res$result, "condition")
  expect_match(conditionMessage(res$result), "produced no output")
})

# run_sim() argument validation ------------------------------------------------

test_that("run_sim rejects the nwpri engine for non-NONMEM tools", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  expect_error(
    run_sim(model = make_model_with_cov(), data = .sim_dat(), id = "sim_test",
            tool = "nlmixr2", n_uncertainty = 10,
            uncertainty_engine = "nwpri",
            fit = list(parameter_estimates = c(POP_CL = 10),
                       covariance_matrix = matrix(1, 1, 1,
                                                  dimnames = list("POP_CL", "POP_CL")))),
    "NONMEM feature"
  )
})

test_that("run_sim rejects the nwpri engine with n_iterations > 1", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  expect_error(
    run_sim(model = make_model_with_cov(), data = .sim_dat(), id = "sim_test",
            tool = "nonmem", n_uncertainty = 10, n_iterations = 5,
            uncertainty_engine = "nwpri",
            fit = list(parameter_estimates = c(POP_CL = 10),
                       covariance_matrix = matrix(1, 1, 1,
                                                  dimnames = list("POP_CL", "POP_CL")))),
    "requires .*n_iterations = 1"
  )
})

test_that("run_sim rejects an unknown uncertainty engine", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()

  expect_error(
    run_sim(model = make_model_with_cov(), data = .sim_dat(), id = "sim_test",
            uncertainty_engine = "magic"),
    "should be one of"
  )
})

# End to end, against a real NONMEM ---------------------------------------------

test_that("run_sim(uncertainty_engine = 'nwpri') runs NONMEM and tags the draws", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  skip_if_nmfe_not_available()

  ## Same fit the record-construction tests are anchored to.
  case <- readRDS(test_path("fixtures", "nwpri_generated_anchor.rds"))$cases$diagonal
  model <- pharmr::read_model_from_string(case$input_code)
  fit <- list(parameter_estimates = case$parameter_estimates,
              covariance_matrix   = case$covariance_matrix)

  ## Two regimens, so the per-regimen run folders and the draw indexing across
  ## them are exercised too.
  regimen <- function(n_ids, label) do.call(rbind, lapply(seq_len(n_ids), function(i) rbind(
    data.frame(ID = i, TIME = 0, DV = 0, AMT = 100, EVID = 1, MDV = 1, CMT = 1,
               .regimen = label),
    data.frame(ID = i, TIME = c(1, 4, 12), DV = 0, AMT = 0, EVID = 0, MDV = 0,
               CMT = 2, .regimen = label)
  )))
  sim_data <- rbind(regimen(2, "A"), regimen(5, "B"))

  out <- run_sim(
    fit = fit, model = model, data = sim_data,
    id = "sim_nwpri", path = withr::local_tempdir(), force = TRUE,
    tool = "nonmem", n_uncertainty = 6, n_chunks = 2,
    uncertainty_engine = "nwpri", seed = 4242, verbose = FALSE
  )

  expect_equal(sort(unique(out$.uncertainty)), 1:6)
  expect_equal(nrow(out), 6 * nrow(sim_data))
  expect_equal(sort(unique(out$regimen_label)), c("A", "B"))
  expect_equal(attr(out, "n_uncertainty_kept"), 6)
  expect_equal(attr(out, "n_uncertainty_requested"), 6)

  ## `T1`..`S11` are the model's own copies of THETA(1..3), OMEGA(1,1) and
  ## SIGMA(1,1), so they show what NONMEM actually drew.
  drawn <- unique(out[, c("regimen_label", ".uncertainty",
                          "T1", "T2", "T3", "O11", "S11")])

  ## Every draw is a different parameter vector: this is the whole point, and
  ## it is what a stale-model bug would break.
  in_a <- drawn[drawn$regimen_label == "A", ]
  expect_equal(nrow(in_a), 6)
  expect_equal(nrow(unique(in_a[, c("T1", "T2", "T3")])), 6)

  ## And draw k is the same parameter vector in both regimens, so
  ## `.uncertainty` pairs across regimens the way it does for the
  ## `"replicates"` engine. (NONMEM draws the prior from its own stream, so the
  ## different number of subjects per regimen does not shift it.)
  in_b <- drawn[drawn$regimen_label == "B", ]
  expect_equal(
    in_a[order(in_a$.uncertainty), c("T1", "T2", "T3", "O11", "S11")],
    in_b[order(in_b$.uncertainty), c("T1", "T2", "T3", "O11", "S11")],
    ignore_attr = TRUE
  )
})
