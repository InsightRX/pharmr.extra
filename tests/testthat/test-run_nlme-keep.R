library(mockery)

# run_nlme(keep = ) / call_pharmpy_tool(keep = ) -------------------------------
#
# The helper is pure R over a folder, so the first block needs neither NONMEM
# nor Pharmpy. The later blocks drive run_nlme() and call_pharmpy_tool() with
# the run stubbed out (the models are still built by Pharmpy, so they are gated
# the way the other stubbed tests are) to check the exit handler is wired.

## Everything NONMEM leaves in a run_nlme() fit folder, taken from the real one
## in tests/testthat/fixtures/run_folder.
.fit_folder_files <- c(
  "data.csv", "final.mod", "patab", "run.clt", "run.coi", "run.cor",
  "run.cov", "run.cpu", "run.ext", "run.grd", "run.lst", "run.mod",
  "run.phi", "run.shk", "run.shm", "run.smt", "run.xml", "sdtab",
  "stderr", "stdout"
)

## ...and the part of it that is the record.
.kept_fit_files <- c(
  "final.mod", "run.cor", "run.cov", "run.ext", "run.lst", "run.mod",
  "run.shk", "stderr", "stdout"
)

.write_fit_folder <- function(folder, files = .fit_folder_files) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  for(f in files) writeLines(paste("contents of", f), file.path(folder, f))
  ## NONMEM's build debris, and a temp folder of its own
  dir.create(file.path(folder, "temp_dir"), showWarnings = FALSE)
  writeLines("", file.path(folder, "temp_dir", "x.o"))
  writeLines("", file.path(folder, "FDATA"))
  folder
}

test_that("keep_nonmem_record keeps a fit's record and drops the rest of the folder", {
  tmp <- withr::local_tempdir()
  staging <- .write_fit_folder(file.path(tmp, "fit1"))
  keep <- file.path(tmp, "kept")

  keep_nonmem_record(staging, keep, pattern = keep_pattern_nonmem_fit)

  expect_setequal(list.files(keep, recursive = TRUE), .kept_fit_files)
  expect_equal(readLines(file.path(keep, "run.lst")), "contents of run.lst")
  expect_equal(readLines(file.path(keep, "final.mod")), "contents of final.mod")
  ## the dataset, the output tables, the iteration files and the build debris
  ## are not part of the record
  expect_false(file.exists(file.path(keep, "data.csv")))
  expect_false(file.exists(file.path(keep, "sdtab")))
  expect_false(file.exists(file.path(keep, "run.phi")))
  expect_false(file.exists(file.path(keep, "FDATA")))
  expect_false(dir.exists(file.path(keep, "temp_dir")))
  ## and the run folder is gone
  expect_false(dir.exists(staging))
})

test_that("keep_nonmem_record leaves run_nlme's summary files beside the run folder alone", {
  ## `<id>_fit_summary.txt`, `<id>_fit_parameters.csv` and `<id>.rds` are
  ## written under `path`, next to the run folder rather than inside it, so
  ## removing the run folder must not take them with it.
  tmp <- withr::local_tempdir()
  staging <- .write_fit_folder(file.path(tmp, "fit1"))
  beside <- c("fit1_fit_summary.txt", "fit1_fit_parameters.csv", "fit1.rds")
  for(f in beside) writeLines("beside", file.path(tmp, f))

  keep_nonmem_record(staging, file.path(tmp, "kept"),
                     pattern = keep_pattern_nonmem_fit)

  expect_true(all(file.exists(file.path(tmp, beside))))
  expect_equal(readLines(file.path(tmp, "fit1.rds")), "beside")
})

test_that("keep_nonmem_record keeps an .ext that a PsN run named after its own stem", {
  tmp <- withr::local_tempdir()
  staging <- .write_fit_folder(file.path(tmp, "fit1"),
                               files = c("run.mod", "run.lst", "psn.ext"))

  keep_nonmem_record(staging, file.path(tmp, "kept"),
                     pattern = keep_pattern_nonmem_fit)

  expect_setequal(list.files(file.path(tmp, "kept"), recursive = TRUE),
                  c("run.mod", "run.lst", "psn.ext"))
})

test_that("keep_nonmem_record's default pattern is still run_sim's", {
  ## The wider fit pattern is opt-in: called without one, the same folder
  ## yields exactly what run_sim() keeps today.
  tmp <- withr::local_tempdir()
  staging <- .write_fit_folder(file.path(tmp, "fit1"))

  keep_nonmem_record(staging, file.path(tmp, "kept"))

  expect_setequal(list.files(file.path(tmp, "kept"), recursive = TRUE),
                  c("run.mod", "run.lst"))
})

test_that("keep_nonmem_record leaves a pre-existing fit folder it has nothing of its own in", {
  tmp <- withr::local_tempdir()
  staging <- file.path(tmp, "fit1")
  dir.create(staging)
  writeLines("mine", file.path(staging, "sentinel.txt"))

  keep_nonmem_record(staging, file.path(tmp, "kept"), created = FALSE,
                     pattern = keep_pattern_nonmem_fit)

  expect_true(dir.exists(staging))
  expect_equal(readLines(file.path(staging, "sentinel.txt")), "mine")
})

# The record of a Pharmpy search -----------------------------------------------

## A tool run folder as call_pharmpy_tool() leaves one: the base fit at the
## root, the final model this package writes, and the tool's own folder with
## one subfolder per candidate fit.
.write_search_folder <- function(folder, tool = "modelsearch") {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  ## the base fit, run by run_nlme() into the tool's run folder
  .write_fit_folder(folder)
  writeLines("$PROBLEM final", file.path(folder, paste0("final_", tool, ".mod")))
  tool_dir <- file.path(folder, paste0(tool, "1"))
  dir.create(tool_dir)
  writeLines("summary", file.path(tool_dir, "results.csv"))
  writeLines("{}", file.path(tool_dir, "results.json"))
  writeLines("<html>", file.path(tool_dir, "results.html"))
  writeLines("{}", file.path(tool_dir, "metadata.json"))
  for(cand in c("input", paste0(tool, "_run1"), "final")) {
    cand_dir <- file.path(tool_dir, "models", cand)
    dir.create(cand_dir, recursive = TRUE)
    for(ext in c("mod", "lst", "ext", "cov")) {
      writeLines(paste(cand, ext), file.path(cand_dir, paste0(cand, ".", ext)))
    }
    writeLines("", file.path(cand_dir, "FDATA"))
    writeLines("", file.path(cand_dir, "sdtab"))
  }
  ## Pharmpy's hidden dataset cache
  dir.create(file.path(tool_dir, "models", ".datasets"), recursive = TRUE)
  writeLines("ID,TIME,DV", file.path(tool_dir, "models", ".datasets", "d1.csv"))
  folder
}

.kept_search_files <- c(
  ## the base fit at the root
  "final.mod", "run.cor", "run.cov", "run.ext", "run.lst", "run.mod",
  "run.shk", "stderr", "stdout",
  ## the final model this package writes
  "final_modelsearch.mod",
  ## the tool's own summaries
  "modelsearch1/results.csv", "modelsearch1/results.json",
  ## and the candidate the search settled on
  "modelsearch1/models/final/final.mod", "modelsearch1/models/final/final.lst",
  "modelsearch1/models/final/final.ext", "modelsearch1/models/final/final.cov"
)

test_that("keep_nonmem_record keeps a search's record and not its candidate fits", {
  tmp <- withr::local_tempdir()
  staging <- .write_search_folder(file.path(tmp, "search1"))
  keep <- file.path(tmp, "kept")

  keep_nonmem_record(staging, keep, pattern = keep_pattern_pharmpy_tool)

  expect_setequal(list.files(keep, recursive = TRUE), .kept_search_files)
  expect_equal(readLines(file.path(keep, "modelsearch1", "models", "final", "final.lst")),
               "final lst")
  ## the candidates the search discarded, and the dataset cache, are not kept
  expect_false(dir.exists(file.path(keep, "modelsearch1", "models", "modelsearch_run1")))
  expect_false(dir.exists(file.path(keep, "modelsearch1", "models", "input")))
  expect_false(dir.exists(file.path(keep, "modelsearch1", "models", ".datasets")))
  ## nor the final candidate's own debris and tables
  expect_false(file.exists(file.path(keep, "modelsearch1", "models", "final", "FDATA")))
  expect_false(file.exists(file.path(keep, "modelsearch1", "models", "final", "sdtab")))
  expect_false(dir.exists(staging))
})

test_that("keep_nonmem_record keeps the simulation tool's candidate folder", {
  ## `tool = "simulation"` writes its candidate to `models/sim`, not
  ## `models/final` (see call_pharmpy_tool()).
  tmp <- withr::local_tempdir()
  staging <- file.path(tmp, "sim1")
  sim_dir <- file.path(staging, "simulation1", "models", "sim")
  dir.create(sim_dir, recursive = TRUE)
  writeLines("$PROBLEM sim", file.path(sim_dir, "sim.mod"))
  writeLines("listing", file.path(sim_dir, "sim.lst"))
  writeLines("TABLE NO. 1", file.path(sim_dir, "simtab"))

  keep_nonmem_record(staging, file.path(tmp, "kept"),
                     pattern = keep_pattern_pharmpy_tool)

  expect_setequal(
    list.files(file.path(tmp, "kept"), recursive = TRUE),
    c("simulation1/models/sim/sim.mod", "simulation1/models/sim/sim.lst")
  )
})

# Through run_nlme(), NONMEM stubbed out ----------------------------------------

## Stand in for prepare_run_folder(): create the run folder and write the
## control stream into it, as the real one does, without Pharmpy or a dataset.
.stub_prepare_run_folder <- function(id, model, path, ...) {
  fit_folder <- create_run_folder(id = id, path = path, verbose = FALSE)
  writeLines("$PROBLEM run", file.path(fit_folder, "run.mod"))
  list(
    model = model,
    model_file = "run.mod",
    output_file = "run.lst",
    fit_folder = fit_folder,
    dataset_path = file.path(fit_folder, "data.csv")
  )
}

test_that("run_nlme(keep = ) keeps the fit record and removes the run folder", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  stub(run_nlme, "prepare_run_folder", .stub_prepare_run_folder)
  ## `check_only` returns right after NM-TRAN, so this exercises a normal
  ## return without having to stub the whole results-reading path.
  stub(run_nlme, "call_nmfe", function(path, ...) {
    .write_fit_folder(path)
    TRUE
  })

  ok <- run_nlme(
    make_model_without_cov(), id = "fit_keep", path = tmp,
    nmfe = "/nonexistent/nmfe", check_only = TRUE, keep = "kept", verbose = FALSE
  )

  expect_true(ok)
  expect_setequal(list.files(file.path(tmp, "kept"), recursive = TRUE),
                  .kept_fit_files)
  expect_false(dir.exists(file.path(tmp, "fit_keep")))
})

test_that("run_nlme(keep = ) keeps the record of a run that aborted", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  stub(run_nlme, "prepare_run_folder", .stub_prepare_run_folder)
  stub(run_nlme, "call_nmfe", function(path, ...) {
    writeLines("NM-TRAN MESSAGES", file.path(path, "run.lst"))
    writeLines("nmfe output", file.path(path, "stdout"))
    writeLines("nmfe: segmentation fault", file.path(path, "stderr"))
    stop("NONMEM fell over")
  })

  expect_error(
    run_nlme(make_model_without_cov(), id = "fit_keep", path = tmp,
             nmfe = "/nonexistent/nmfe", keep = "kept", verbose = FALSE),
    "NONMEM fell over"
  )

  expect_equal(readLines(file.path(tmp, "kept", "run.lst")), "NM-TRAN MESSAGES")
  expect_true(file.exists(file.path(tmp, "kept", "run.mod")))
  expect_true(file.exists(file.path(tmp, "kept", "stdout")))
  ## the reason an nmfe run fell over is only in stderr
  expect_equal(readLines(file.path(tmp, "kept", "stderr")),
               "nmfe: segmentation fault")
  expect_false(dir.exists(file.path(tmp, "fit_keep")))
})

test_that("run_nlme(keep = NULL) leaves the run folder in place", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  stub(run_nlme, "prepare_run_folder", .stub_prepare_run_folder)
  stub(run_nlme, "call_nmfe", function(path, ...) {
    .write_fit_folder(path)
    TRUE
  })

  run_nlme(make_model_without_cov(), id = "fit_keep", path = tmp,
           nmfe = "/nonexistent/nmfe", check_only = TRUE, verbose = FALSE)

  expect_true(file.exists(file.path(tmp, "fit_keep", "run.lst")))
  expect_true(file.exists(file.path(tmp, "fit_keep", "data.csv")))
})

test_that("run_nlme(keep = ) is checked before anything runs", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  ran <- FALSE
  stub(run_nlme, "prepare_run_folder", function(...) {
    ran <<- TRUE
    stop("should not be reached")
  })
  args <- list(model = make_model_without_cov(), id = "fit_keep", path = tmp,
               nmfe = "/nonexistent/nmfe", verbose = FALSE)

  expect_error(do.call(run_nlme, c(args, list(keep = c("a", "b")))),
               "single non-empty string")
  expect_error(do.call(run_nlme, c(args, list(keep = ""))),
               "single non-empty string")
  expect_error(
    do.call(run_nlme, c(args, list(keep = file.path("fit_keep", "kept")))),
    "must not be the run folder or lie inside it"
  )
  expect_false(ran)
  expect_false(dir.exists(file.path(tmp, "fit_keep")))
})

test_that("run_nlme(keep = ) refuses as_job, which returns before NONMEM has finished", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  ran <- FALSE
  stub(run_nlme, "prepare_run_folder", function(...) {
    ran <<- TRUE
    stop("should not be reached")
  })

  expect_error(
    run_nlme(make_model_without_cov(), id = "fit_keep", path = tmp,
             method = "pharmpy", as_job = TRUE, keep = "kept", verbose = FALSE),
    "cannot be combined with"
  )
  expect_false(ran)
})

test_that("run_nlme(keep = ) leaves an existing run folder alone when a check fails", {
  ## The handler removes the run folder, so it must not be armed while
  ## arguments are still being checked: an error there would take an existing
  ## `id` folder -- an earlier run, or whatever else is kept in it -- with it.
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  .write_fit_folder(file.path(tmp, "fit_keep"))
  writeLines("mine", file.path(tmp, "fit_keep", "sentinel.txt"))

  ## No `force`, so create_run_folder() refuses the existing folder.
  expect_error(
    run_nlme(make_model_without_cov(), id = "fit_keep", path = tmp,
             nmfe = "/nonexistent/nmfe", keep = "kept", verbose = FALSE),
    "Use `force` to overwrite"
  )

  expect_true(dir.exists(file.path(tmp, "fit_keep")))
  expect_equal(readLines(file.path(tmp, "fit_keep", "sentinel.txt")), "mine")
  expect_false(dir.exists(file.path(tmp, "kept")))
})

# Through call_pharmpy_tool(), Pharmpy stubbed out -----------------------------

test_that("call_pharmpy_tool(keep = ) keeps the search record of a run that aborted", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  ## Stand in for the tool: leave a search folder behind, then fall over.
  stub(call_pharmpy_tool, "withr::with_dir", function(new, code) {
    .write_search_folder(new)
    stop("pharmpy fell over")
  })

  expect_error(
    call_pharmpy_tool(id = "search_keep", model = make_model_without_cov(),
                      tool = "bootstrap", keep = "kept", verbose = FALSE),
    "Pharmpy error running bootstrap"
  )

  expect_setequal(list.files(file.path(tmp, "kept"), recursive = TRUE),
                  .kept_search_files)
  expect_false(dir.exists(file.path(tmp, "search_keep")))
})

test_that("call_pharmpy_tool(keep = NULL) leaves the run folder in place", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  stub(call_pharmpy_tool, "remove_tables_from_model", function(m, ...) m)
  stub(call_pharmpy_tool, "clean_pharmpy_runfolders", function(...) invisible(NULL))
  stub(call_pharmpy_tool, "withr::with_dir", function(new, code) {
    .write_search_folder(new)
    stop("pharmpy fell over")
  })

  expect_error(
    call_pharmpy_tool(id = "search_keep", model = make_model_without_cov(),
                      tool = "bootstrap", verbose = FALSE),
    "Pharmpy error running bootstrap"
  )

  expect_true(dir.exists(file.path(tmp, "search_keep", "modelsearch1")))
  expect_false(dir.exists(file.path(tmp, "kept")))
})

test_that("call_pharmpy_tool(keep = ) is checked before anything runs", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  ran <- FALSE
  stub(call_pharmpy_tool, "create_run_folder", function(...) {
    ran <<- TRUE
    stop("should not be reached")
  })
  args <- list(id = "search_keep", model = make_model_without_cov(),
               tool = "bootstrap", verbose = FALSE)

  expect_error(do.call(call_pharmpy_tool, c(args, list(keep = c("a", "b")))),
               "single non-empty string")
  expect_error(
    do.call(call_pharmpy_tool,
            c(args, list(keep = file.path("search_keep", "kept")))),
    "must not be the run folder or lie inside it"
  )
  expect_false(ran)
  expect_false(dir.exists(file.path(tmp, "search_keep")))
})
