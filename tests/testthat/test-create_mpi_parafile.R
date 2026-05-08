test_that("create_mpi_parafile writes a parafile and returns absolute path", {
  tmp <- withr::local_tempdir()
  out <- create_mpi_parafile(path = tmp, threads = 4)

  expect_true(file.exists(out))
  expect_identical(normalizePath(out), out)
  expect_true(grepl("parafile\\.pnm$", out))

  contents <- readLines(out)
  expect_true(any(grepl("^\\$GENERAL", contents)))
  expect_true(any(grepl("^\\$COMMANDS", contents)))
  expect_true(any(grepl("^\\$DIRECTORIES", contents)))
  expect_true(any(grepl("^\\$DEFAULTS", contents)))
  expect_true(any(grepl("^\\[nodes\\]=4$", contents)))
  expect_true(any(grepl("NODES=\\[nodes\\]", contents)))
  ## MPI parafile: must request MPI transport and launch via mpirun
  expect_true(any(grepl("TRANSFER_TYPE=1", contents)))
  expect_true(any(grepl("PARSE_TYPE=4", contents)))
  expect_true(any(grepl("mpirun", contents)))
  expect_true(any(grepl("2-\\[nodes\\]:\\s*-wdir", contents)))
})

test_that("create_mpi_parafile honours custom filename", {
  tmp <- withr::local_tempdir()
  out <- create_mpi_parafile(path = tmp, threads = 2, filename = "my.pnm")

  expect_true(file.exists(out))
  expect_true(grepl("my\\.pnm$", out))
})

test_that("create_mpi_parafile rejects invalid threads", {
  tmp <- withr::local_tempdir()
  expect_error(create_mpi_parafile(tmp, threads = 0), "positive integer")
  expect_error(create_mpi_parafile(tmp, threads = 1.5), "positive integer")
  expect_error(create_mpi_parafile(tmp, threads = "two"), "positive integer")
})

test_that("create_mpi_parafile errors when path does not exist", {
  expect_error(
    create_mpi_parafile(file.path(tempdir(), "does_not_exist_xyz"), threads = 2),
    "does not exist"
  )
})
