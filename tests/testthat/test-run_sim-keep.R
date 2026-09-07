# run_sim(keep = ): keep run.mod/run.lst, remove the run folder -----------------
#
# The helper is pure R over a folder, so the first block needs neither NONMEM
# nor Pharmpy. The second block drives run_sim() itself with NONMEM stubbed out
# (the control streams are still rendered by Pharmpy, so it is gated the way
# the other stubbed run_sim() tests are) to check the exit handler is wired.

## A run folder as run_sim() leaves one: a regimen folder and a replicate's
## regimen folder, each with the two files worth keeping and the debris that
## is not.
.make_run_folder <- function(root, id = "sim_x") {
  staging <- file.path(root, id)
  reg <- file.path(staging, "regimen_1")
  unc <- file.path(staging, "uncertainty_2", "regimen_1")
  dir.create(file.path(reg, "temp_dir"), recursive = TRUE)
  dir.create(unc, recursive = TRUE)
  writeLines("$PROBLEM regimen 1", file.path(reg, "run.mod"))
  writeLines("listing regimen 1", file.path(reg, "run.lst"))
  writeLines("ID,TIME,DV", file.path(reg, "data.csv"))
  writeLines("TABLE NO. 1", file.path(reg, "simtab"))
  writeLines("", file.path(reg, "temp_dir", "x.o"))
  writeLines("$PROBLEM replicate 2", file.path(unc, "run.mod"))
  writeLines("listing replicate 2", file.path(unc, "run.lst"))
  staging
}

.kept_files <- c(
  "regimen_1/run.lst", "regimen_1/run.mod",
  "uncertainty_2/regimen_1/run.lst", "uncertainty_2/regimen_1/run.mod"
)

test_that("keep_nonmem_record copies run.mod/run.lst out and removes the run folder", {
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)
  keep <- file.path(tmp, "kept")

  keep_nonmem_record(staging, keep)

  expect_setequal(list.files(keep, recursive = TRUE), .kept_files)
  expect_equal(readLines(file.path(keep, "regimen_1", "run.mod")),
               "$PROBLEM regimen 1")
  expect_equal(readLines(file.path(keep, "uncertainty_2", "regimen_1", "run.lst")),
               "listing replicate 2")
  ## none of the debris came along
  expect_false(file.exists(file.path(keep, "regimen_1", "data.csv")))
  expect_false(file.exists(file.path(keep, "regimen_1", "simtab")))
  expect_false(dir.exists(file.path(keep, "regimen_1", "temp_dir")))
  ## and the run folder is gone
  expect_false(dir.exists(staging))
})

test_that("keep_nonmem_record resolves a relative `keep` against the working directory", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  staging <- .make_run_folder(tmp)

  out <- keep_nonmem_record(staging, "kept")

  expect_equal(out, normalizePath(file.path(tmp, "kept")))
  expect_setequal(list.files(file.path(tmp, "kept"), recursive = TRUE), .kept_files)
  expect_false(dir.exists(staging))
})

test_that("keep_nonmem_record writes into an existing `keep` and overwrites only name collisions", {
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)
  keep <- file.path(tmp, "kept")
  dir.create(file.path(keep, "regimen_1"), recursive = TRUE)
  writeLines("stale listing", file.path(keep, "regimen_1", "run.lst"))
  writeLines("untouched", file.path(keep, "notes.txt"))

  keep_nonmem_record(staging, keep)

  expect_equal(readLines(file.path(keep, "regimen_1", "run.lst")),
               "listing regimen 1")
  expect_equal(readLines(file.path(keep, "notes.txt")), "untouched")
  expect_setequal(list.files(keep, recursive = TRUE), c(.kept_files, "notes.txt"))
})

test_that("keep_nonmem_record aborts when `keep` is the run folder or lies inside it", {
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)
  before <- list.files(staging, recursive = TRUE)

  expect_error(keep_nonmem_record(staging, file.path(staging, "kept")),
               "must not be the run folder or lie inside it")
  expect_error(keep_nonmem_record(staging, staging),
               "must not be the run folder or lie inside it")
  ## a relative path inside it is caught the same way
  withr::with_dir(staging, expect_error(keep_nonmem_record(staging, "kept"),
                                        "must not be the run folder"))
  ## and nothing was copied or removed
  expect_equal(list.files(staging, recursive = TRUE), before)
  expect_false(dir.exists(file.path(staging, "kept")))
})

test_that("keep_nonmem_record rejects a `keep` that is not a single non-empty string", {
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)

  expect_error(keep_nonmem_record(staging, ""), "single non-empty string")
  expect_error(keep_nonmem_record(staging, c("a", "b")), "single non-empty string")
  expect_error(keep_nonmem_record(staging, NA_character_), "single non-empty string")
  expect_error(keep_nonmem_record(staging, 1), "single non-empty string")
  expect_true(dir.exists(staging))
})

test_that("keep = NULL leaves the run folder untouched", {
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)
  before <- list.files(staging, recursive = TRUE)

  expect_null(keep_nonmem_record(staging, NULL))

  expect_equal(list.files(staging, recursive = TRUE), before)
})

test_that("keep_nonmem_record copes with a run folder that was never created", {
  ## A run that aborts before its first run folder is written still runs the
  ## exit handler: nothing to keep, nothing to remove, but `keep` exists.
  tmp <- withr::local_tempdir()
  keep <- file.path(tmp, "kept")

  expect_no_error(keep_nonmem_record(file.path(tmp, "sim_never"), keep))

  expect_true(dir.exists(keep))
  expect_length(list.files(keep, recursive = TRUE), 0)
})

test_that("keep_nonmem_record leaves a pre-existing run folder it has nothing of its own in", {
  ## `created = FALSE`: the run folder was already there when the run started
  ## and the run never wrote a control stream into it (it aborted before
  ## reaching NONMEM), so the folder is someone else's to keep.
  tmp <- withr::local_tempdir()
  staging <- file.path(tmp, "sim_x")
  dir.create(staging)
  writeLines("mine", file.path(staging, "sentinel.txt"))
  keep <- file.path(tmp, "kept")

  keep_nonmem_record(staging, keep, created = FALSE)

  expect_true(dir.exists(staging))
  expect_equal(readLines(file.path(staging, "sentinel.txt")), "mine")
  expect_length(list.files(keep, recursive = TRUE), 0)
})

test_that("keep_nonmem_record still records a pre-existing run folder the run wrote into", {
  ## Same `created = FALSE`, but NONMEM did run here: the record is copied out
  ## and the folder removed, as with any other run.
  tmp <- withr::local_tempdir()
  staging <- .make_run_folder(tmp)
  writeLines("stale", file.path(staging, "sentinel.txt"))
  keep <- file.path(tmp, "kept")

  keep_nonmem_record(staging, keep, created = FALSE)

  expect_setequal(list.files(keep, recursive = TRUE), .kept_files)
  expect_false(dir.exists(staging))
})

test_that("validate_keep_folder refuses a run folder that is not inside its base", {
  ## `id = "."` (or `""`) makes the run folder the working directory itself,
  ## which `keep` then removes.
  tmp <- withr::local_tempdir()
  keep <- file.path(withr::local_tempdir(), "kept")
  expect_error(
    validate_keep_folder(keep, staging = file.path(tmp, "."), base = tmp),
    "not a subfolder"
  )
  expect_error(
    validate_keep_folder(keep, staging = file.path(tmp, ""), base = tmp),
    "not a subfolder"
  )
  expect_equal(
    validate_keep_folder(keep, staging = file.path(tmp, "sim_x"), base = tmp),
    absolute_path(keep)
  )
})

test_that("absolute_path resolves the existing ancestors of a path that does not exist", {
  tmp <- withr::local_tempdir()
  real <- normalizePath(tmp)
  expect_equal(absolute_path(file.path(tmp, "new", "deeper")),
               file.path(real, "new", "deeper"))
  expect_equal(absolute_path(file.path(tmp, "a", "..", "b")), file.path(real, "b"))
  withr::with_dir(tmp, expect_equal(absolute_path("kept"), file.path(real, "kept")))
})

# Through run_sim(), NONMEM stubbed out ----------------------------------------

.keep_fake_fit <- function() {
  list(parameter_estimates = c(POP_CL = 1, POP_V = 10),
       covariance_matrix = diag(2))
}

## Stub the draw (the fake fit's covariance matrix has no names for pharmpy to
## sample from) and the per-replicate model update, as the other stubbed
## run_sim() tests do.
.local_keep_stubs <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    sample_uncertainty_parameters =
      function(model, parameter_estimates, covariance_matrix, n, seed) {
        as.data.frame(matrix(rep(seq_len(n), 2), ncol = 2,
                             dimnames = list(NULL, c("POP_CL", "POP_V"))))
      },
    .package = "pharmr.extra",
    .env = .local_envir
  )
  testthat::local_mocked_bindings(
    set_initial_estimates = function(model, inits) model,
    .package = "pharmr",
    .env = .local_envir
  )
}

test_that("run_sim(keep = ) keeps every replicate's record and removes the run folder", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  .local_keep_stubs()
  ## Stand in for NONMEM: leave a listing next to the control stream.
  local_mock_nonmem_sim(function(spec, nmfe, table_names, clean = TRUE) {
    writeLines("listing", file.path(spec$folder, "run.lst"))
    .mock_sim_tab()
  })

  out <- run_sim(
    fit = .keep_fake_fit(), model = make_model_without_cov(), data = .sim_dat(),
    id = "sim_keep", n_uncertainty = 2, uncertainty_engine = "replicates",
    keep = "kept", verbose = FALSE
  )

  expect_equal(sort(unique(out$.uncertainty)), 1:2)
  expect_setequal(
    list.files(file.path(tmp, "kept"), recursive = TRUE),
    c("uncertainty_1/regimen_1/run.lst", "uncertainty_1/regimen_1/run.mod",
      "uncertainty_2/regimen_1/run.lst", "uncertainty_2/regimen_1/run.mod")
  )
  expect_match(readLines(file.path(tmp, "kept", "uncertainty_1", "regimen_1", "run.mod")),
               "^\\$PROBLEM", all = FALSE)
  expect_false(dir.exists(file.path(tmp, "sim_keep")))
})

test_that("run_sim(keep = ) keeps the listing of a run that aborted", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  .local_keep_stubs()
  local_mock_nonmem_sim(function(spec, nmfe, table_names, clean = TRUE) {
    writeLines("NM-TRAN MESSAGES", file.path(spec$folder, "run.lst"))
    stop("NONMEM fell over")
  })

  expect_error(
    run_sim(
      fit = .keep_fake_fit(), model = make_model_without_cov(), data = .sim_dat(),
      id = "sim_keep", n_uncertainty = 2, uncertainty_engine = "replicates",
      keep = "kept", verbose = FALSE
    ),
    "Uncertainty replicate 1 failed"
  )

  expect_equal(readLines(file.path(tmp, "kept", "uncertainty_1", "regimen_1", "run.lst")),
               "NM-TRAN MESSAGES")
  expect_true(file.exists(file.path(tmp, "kept", "uncertainty_1", "regimen_1", "run.mod")))
  expect_false(dir.exists(file.path(tmp, "sim_keep")))
})

test_that("run_sim(keep = NULL) leaves the run folder in place", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  .local_keep_stubs()
  local_mock_nonmem_sim()

  run_sim(
    fit = .keep_fake_fit(), model = make_model_without_cov(), data = .sim_dat(),
    id = "sim_keep", n_uncertainty = 2, uncertainty_engine = "replicates",
    verbose = FALSE
  )

  expect_true(file.exists(file.path(tmp, "sim_keep", "uncertainty_2", "regimen_1", "run.mod")))
})

test_that("run_sim(keep = ) is checked before anything runs", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  ran <- FALSE
  local_mocked_bindings(
    run_nlme = function(...) { ran <<- TRUE; .mock_nlme_result() },
    .package = "pharmr.extra"
  )
  args <- list(model = make_model_without_cov(), data = .sim_dat(),
               verbose = FALSE)

  expect_error(do.call(run_sim, c(args, list(id = "sim_keep", keep = c("a", "b")))),
               "single non-empty string")
  expect_error(do.call(run_sim, c(args, list(id = "sim_keep",
                                             keep = file.path("sim_keep", "kept")))),
               "must not be the run folder or lie inside it")
  ## `id = "."` would make the working directory the run folder to remove
  expect_error(do.call(run_sim, c(args, list(id = ".", keep = "kept"))),
               "not a subfolder")
  expect_false(ran)
  expect_false(dir.exists(file.path(tmp, "sim_keep")))
})

test_that("run_sim(keep = ) leaves an existing run folder alone when a check fails", {
  ## The `keep` handler removes the run folder, so it must not be armed while
  ## arguments are still being checked: an error there would take an existing
  ## `id` folder -- an earlier run, or whatever else the user keeps in it --
  ## with it.
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  dir.create(file.path(tmp, "sim_keep"))
  writeLines("mine", file.path(tmp, "sim_keep", "sentinel.txt"))

  expect_error(
    run_sim(model = make_model_without_cov(), data = matrix(1:4, ncol = 2),
            id = "sim_keep", keep = "kept", verbose = FALSE),
    "must be a data.frame"
  )

  expect_true(dir.exists(file.path(tmp, "sim_keep")))
  expect_equal(readLines(file.path(tmp, "sim_keep", "sentinel.txt")), "mine")
})

test_that("run_sim(keep = ) is accepted, and does nothing, on the nlmixr2 backend", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  local_mocked_bindings(
    run_sim_nlmixr = function(...) .mock_sim_table(),
    .package = "pharmr.extra"
  )

  out <- run_sim(model = make_model_without_cov(), data = .sim_dat(),
                 tool = "nlmixr2", keep = "kept", verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_false(dir.exists(file.path(tmp, "kept")))
})
