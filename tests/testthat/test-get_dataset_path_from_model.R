## get_dataset_path_from_model() only reads `model$code`, so a plain list
## stands in for a Pharmpy model.
fake_model <- function(data_record) {
  list(code = paste0(
    "$PROBLEM Test\n$INPUT ID TIME DV\n", data_record, "\n$PK\nCL=THETA(1)\n"
  ))
}

test_that("get_dataset_path_from_model returns NULL when $DATA is not on disk", {
  dir <- withr::local_tempdir()
  expect_null(get_dataset_path_from_model(fake_model("$DATA DUMMYPATH"), base_dir = dir))
})

test_that("get_dataset_path_from_model resolves a relative path against base_dir", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "data.csv"))
  res <- get_dataset_path_from_model(
    fake_model("$DATA data.csv IGNORE=@"),
    base_dir = dir
  )
  expect_equal(as.character(res), file.path(dir, "data.csv"))
  expect_false(attr(res, "absolute"))
})

test_that("get_dataset_path_from_model flags absolute paths and strips quotes", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "data.csv")
  file.create(path)
  res <- get_dataset_path_from_model(
    fake_model(paste0("$DATA '", path, "' IGNORE=@")),
    base_dir = tempdir()
  )
  expect_equal(as.character(res), path)
  expect_true(attr(res, "absolute"))
})

test_that("get_dataset_path_from_model ignores IGNORE=/ACCEPT= options and directories", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "IGNORE=@"))
  dir.create(file.path(dir, "subdir"))
  expect_null(get_dataset_path_from_model(
    fake_model("$DATA subdir IGNORE=@"),
    base_dir = dir
  ))
})
