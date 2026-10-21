test_that("detect_nmfe_version finds the nmfe script in the run folder", {
  nm_path <- withr::local_tempdir()
  dir.create(file.path(nm_path, "run"))
  file.create(file.path(nm_path, "run", c("nmfe75", "other.sh")))
  expect_equal(detect_nmfe_version(nm_path), "nmfe75")
})

test_that("detect_nmfe_version returns NA when no nmfe script is present", {
  nm_path <- withr::local_tempdir()
  dir.create(file.path(nm_path, "run"))
  file.create(file.path(nm_path, "run", "other.sh"))
  expect_true(is.na(detect_nmfe_version(nm_path)))
})

test_that("get_nmfe_location uses the nmfe detected in the configured NONMEM folder", {
  nm_path <- withr::local_tempdir()
  dir.create(file.path(nm_path, "run"))
  file.create(file.path(nm_path, "run", "nmfe76"))
  local_mocked_bindings(
    get_pharmpy_conf = function(...) {
      list(pharmpy.plugins.nonmem = list(default_nonmem_path = nm_path))
    }
  )
  expect_equal(get_nmfe_location(), file.path(nm_path, "run", "nmfe76"))
})
