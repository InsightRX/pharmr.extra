# NONMEM backend of the "replicates" uncertainty engine (#129) ----------------
#
# The prepare half needs Pharmpy (it applies the draw and renders the control
# stream) and is gated on `skip_if_nonmem_not_available()`. The execute half is
# pure R -- run NONMEM, read the table back -- so it is tested here against a
# mocked-out `call_nmfe()` and needs neither NONMEM nor Pharmpy, which is the
# whole point of the split: that half is what runs in the worker processes.

# Regimen splitting ------------------------------------------------------------

.two_regimen_dat <- function() {
  rbind(
    data.frame(ID = 1, TIME = c(0, 6), DV = 0, AMT = c(100, 0),
               EVID = c(1, 0), MDV = c(1, 0), .regimen = "100 mg"),
    data.frame(ID = 1, TIME = c(0, 6), DV = 0, AMT = c(200, 0),
               EVID = c(1, 0), MDV = c(1, 0), .regimen = "200 mg")
  )
}

test_that("resolve_sim_regimens splits the dataset by regimen", {
  regs <- resolve_sim_regimens(.two_regimen_dat(), input_data = NULL,
                               verbose = FALSE)

  expect_length(regs, 2)
  expect_equal(vapply(regs, function(r) r$index, numeric(1)), 1:2)
  expect_equal(vapply(regs, function(r) r$label, character(1)),
               c("100 mg", "200 mg"))
  ## `.regimen` is a run_sim() bookkeeping column, not something NONMEM should
  ## see in the dataset it simulates from
  expect_false(any(vapply(regs, function(r) ".regimen" %in% names(r$data),
                          logical(1))))
  ## and the doses `calc_pk_variables()` needs come along per regimen
  expect_equal(regs[[1]]$regimen_for_pk, list(dose = 100))
  expect_equal(regs[[2]]$regimen_for_pk, list(dose = 200))
})

test_that("resolve_sim_regimens labels an unlabelled dataset as one regimen", {
  regs <- resolve_sim_regimens(.sim_dat(), input_data = NULL, verbose = FALSE)
  expect_length(regs, 1)
  expect_equal(regs[[1]]$label, "original regimens")
})

test_that("resolve_sim_regimens sorts records and follows the model's columns", {
  ## deliberately out of order, and dosing recorded after the observation at
  ## the same time point
  dat <- data.frame(
    TIME = c(6, 0, 0), DV = c(5, 0, 0), ID = c(1, 1, 1),
    AMT = c(0, 0, 100), EVID = c(0, 0, 1), MDV = c(0, 1, 1)
  )
  regs <- resolve_sim_regimens(
    dat, input_data = data.frame(ID = 1, TIME = 0, DV = 0, AMT = 0,
                                 EVID = 0, MDV = 0),
    verbose = FALSE
  )
  reg_data <- regs[[1]]$data
  ## NONMEM needs the dose record before the observation it shares a time with
  expect_equal(reg_data$EVID, c(1, 0, 0))
  expect_equal(reg_data$TIME, c(0, 0, 6))
  ## column order follows the model's dataset, since NONMEM reads $INPUT by
  ## position
  expect_equal(names(reg_data), c("ID", "TIME", "DV", "AMT", "EVID", "MDV"))
})

test_that("sim_regimen_doses returns NULL when there is nothing to derive", {
  expect_null(sim_regimen_doses(data.frame(ID = 1, TIME = 0, DV = 1)))
  ## dose columns present, but no dose records
  expect_null(sim_regimen_doses(
    data.frame(ID = 1, TIME = 0, DV = 1, AMT = 0, EVID = 0)
  ))
  expect_equal(
    sim_regimen_doses(data.frame(ID = 1, TIME = c(0, 1), DV = 0,
                                 AMT = c(50, 0), EVID = c(1, 0))),
    list(dose = 50)
  )
})

# Execute half -----------------------------------------------------------------

## A prepared run folder, as prepare_nonmem_replicate_specs() leaves it: the
## control stream and dataset are already written, so the worker only runs
## NONMEM and reads the table back.
.prepared_regimen <- function(label = "original regimens", dose = 100) {
  folder <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("$PROBLEM sim", file.path(folder, "run.mod"))
  list(
    label = label, folder = folder, model_file = "run.mod",
    output_file = "run.lst", regimen_for_pk = list(dose = dose)
  )
}

## Stand in for NONMEM: write a `simtab` into the run folder it is pointed at.
.nmfe_writing <- function(table = "simtab", cl = 5.1) {
  function(model_file, output_file, path, ...) {
    writeLines(
      c("TABLE NO.  1",
        " ID          TIME        DV          PRED        CL",
        sprintf("  1.0000E+00  %sE+00  4.8000E+00  4.7000E+00  %sE+00",
                c("0.0000", "6.0000"), format(cl, nsmall = 4))),
      file.path(path, table)
    )
    invisible(NULL)
  }
}

test_that("run_nonmem_sim_folder runs NONMEM and reads the table back", {
  spec <- .prepared_regimen()
  local_mocked_bindings(call_nmfe = .nmfe_writing(), .package = "pharmr.extra")

  tab <- run_nonmem_sim_folder(spec, nmfe = "/nonexistent/nmfe",
                               table_names = "simtab")

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 2)
  expect_true(all(c("ID", "TIME", "DV", "PRED", "CL") %in% names(tab)))
})

test_that("run_nonmem_sim_folder surfaces a simulation that wrote no table", {
  spec <- .prepared_regimen(label = "100 mg")
  writeLines(c("AN ERROR WAS FOUND IN THE CONTROL STATEMENTS", " bad $SIM"),
             file.path(spec$folder, "run.lst"))
  ## `call_nmfe()` returning quietly without writing a table is exactly the
  ## silent failure the replicate path has to catch itself.
  local_mocked_bindings(call_nmfe = function(...) invisible(NULL),
                        .package = "pharmr.extra")

  expect_error(
    run_nonmem_sim_folder(spec, nmfe = "/nonexistent/nmfe",
                          table_names = "simtab"),
    class = "pharmr_extra_sim_failed"
  )
})

test_that("a NONMEM replicate runs every regimen and labels the output", {
  spec <- list(
    index = 2, table_names = "simtab",
    regimens = list(.prepared_regimen("100 mg", dose = 100),
                    .prepared_regimen("200 mg", dose = 200))
  )
  local_mocked_bindings(call_nmfe = .nmfe_writing(), .package = "pharmr.extra")

  res <- make_nonmem_replicate_fn(nmfe = "/nonexistent/nmfe")(spec)

  expect_equal(res$index, 2)
  expect_s3_class(res$result, "data.frame")
  ## both regimens, in spec order, each tagged with its own label
  expect_equal(unique(res$result$regimen_label), c("100 mg", "200 mg"))
  expect_equal(nrow(res$result), 4)
})

test_that("a NONMEM replicate adds PK variables per regimen when asked", {
  spec <- list(
    index = 1, table_names = "simtab",
    regimens = list(.prepared_regimen("100 mg", dose = 100),
                    .prepared_regimen("200 mg", dose = 200))
  )
  local_mocked_bindings(call_nmfe = .nmfe_writing(), .package = "pharmr.extra")

  res <- make_nonmem_replicate_fn(
    nmfe = "/nonexistent/nmfe", add_pk_variables = TRUE
  )(spec)

  expect_true(all(c("CMAX_OBS", "AUC_SS") %in% names(res$result)))
  ## AUC_SS is dose/CL, so it must use *that* regimen's dose rather than the
  ## first one's
  auc <- tapply(res$result$AUC_SS, res$result$regimen_label, unique)
  expect_equal(as.numeric(auc[["200 mg"]]) / as.numeric(auc[["100 mg"]]), 2)
})

test_that("a failing NONMEM replicate comes back as a value, not an error", {
  spec <- list(
    index = 3, table_names = "simtab",
    regimens = list(.prepared_regimen())
  )
  local_mocked_bindings(
    call_nmfe = function(...) stop("nmfe exploded"),
    .package = "pharmr.extra"
  )

  ## Errors are captured so one bad replicate cannot take a worker (and with it
  ## the whole run) down; what to do about it is the caller's decision.
  res <- make_nonmem_replicate_fn(nmfe = "/nonexistent/nmfe")(spec)
  expect_equal(res$index, 3)
  expect_s3_class(res$result, "condition")
  expect_match(conditionMessage(res$result), "nmfe exploded")
})

test_that("abort_on_failed_replicates stops on the first failure only", {
  ok <- list(index = 1, result = data.frame(x = 1), warnings = list())
  bad <- list(index = 2, result = simpleError("no output table"),
              warnings = list())
  bad2 <- list(index = 3, result = simpleError("licence expired"),
               warnings = list())

  expect_silent(abort_on_failed_replicates(list(ok)))
  expect_error(abort_on_failed_replicates(list(ok, bad, bad2)),
               "Uncertainty replicate 2 failed")
})

test_that("abort_on_failed_replicates re-emits what the worker warned about", {
  captured <- run_captured(2, function() {
    warning("dataset looks odd")
    stop("no output table")
  })
  expect_warning(
    try(abort_on_failed_replicates(list(captured)), silent = TRUE),
    "Uncertainty replicate 2: dataset looks odd"
  )
})

# Prepare half (needs Pharmpy) -------------------------------------------------

test_that("prepare_nonmem_replicate_specs gives every replicate its own folder", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  path <- withr::local_tempdir()

  mod <- make_model_without_cov()
  draws <- data.frame(POP_CL = c(1, 2), POP_V = c(10, 20))
  specs <- prepare_nonmem_replicate_specs(
    model = mod,
    draws = draws,
    regimens = resolve_sim_regimens(.two_regimen_dat(), input_data = NULL,
                                    verbose = FALSE),
    id = "sim_test",
    path = path,
    seed = 1234,
    n_iterations = 1,
    verbose = FALSE
  )

  expect_length(specs, 2)
  expect_equal(vapply(specs, function(s) s$index, numeric(1)), 1:2)
  ## One folder per replicate *and* regimen: reusing `id/regimen_<i>` for every
  ## draw is what concurrent replicates would clobber.
  folders <- unlist(lapply(specs, function(s) {
    vapply(s$regimens, function(r) r$folder, character(1))
  }))
  expect_length(unique(folders), 4)
  expect_true(all(dir.exists(folders)))
  expect_match(folders, "uncertainty_[12]/regimen_[12]$")
  ## and each is ready to run: control stream plus its own copy of the dataset
  ## (NM-TRAN truncates long $DATA paths, so a shared one is not an option)
  expect_true(all(file.exists(file.path(folders, "run.mod"))))
  expect_true(all(file.exists(file.path(folders, "data.csv"))))

  ## The workers get the table names, so they never need the model object
  expect_equal(specs[[1]]$table_names, "simtab")

  ## Nothing Python travels to the worker: the specs must survive a round trip
  ## through serialisation, which a Pharmpy model object would not.
  expect_equal(unserialize(serialize(specs, NULL)), specs)
})

test_that("prepare_nonmem_replicate_specs applies each draw to its replicate", {
  local_pharmr.extra_options()
  skip_if_nonmem_not_available()
  path <- withr::local_tempdir()

  mod <- make_model_without_cov()
  draws <- data.frame(POP_CL = c(11, 22), POP_V = c(44, 55))
  specs <- prepare_nonmem_replicate_specs(
    model = mod, draws = draws,
    regimens = resolve_sim_regimens(.sim_dat(), input_data = NULL,
                                    verbose = FALSE),
    id = "sim_draws", path = path, seed = 1, n_iterations = 1, verbose = FALSE
  )

  ## The draw reaches NONMEM through $THETA in the control stream that was
  ## written, which is the only place a worker could pick it up from.
  code <- lapply(specs, function(s) {
    paste(readLines(file.path(s$regimens[[1]]$folder, "run.mod")),
          collapse = "\n")
  })
  expect_match(code[[1]], "11")
  expect_match(code[[2]], "22")
  expect_false(identical(code[[1]], code[[2]]))
})
