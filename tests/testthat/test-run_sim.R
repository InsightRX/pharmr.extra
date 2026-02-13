# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

skip_on_ci()

test_that("Basic simulation works (using `model` argument, not `fit`)", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())
  
  mod <- pharmr::load_example_model("pheno")
  pharmr::load_dataset(mod)
  dat <- mod$dataset |>
    as.data.frame() |>
    dplyr::mutate(
      EVID = ifelse(AMT == 0, 0, 1),
      MDV = ifelse(DV == 0, 1, 0),
      CMT = 1
    )
  out <- run_sim(
    model = mod, 
    data = dat,
    variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED")
  )
  expect_equal(dim(out), c(744, 12))
})

test_that("Basic simulation works (using model file specified to `model`)", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())

  mod <- pharmr::load_example_model("pheno")
  model_code <- mod$code
  pharmr::load_dataset(mod)
  dat <- mod$dataset |>
    as.data.frame() |>
    dplyr::mutate(
      EVID = ifelse(AMT == 0, 0, 1),
      MDV = ifelse(DV == 0, 1, 0),
      CMT = 1
    )
  # Write model code to a temp file (run_sim now expects a filename)
  tmp_mod <- tempfile(fileext = ".mod")
  writeLines(model_code, tmp_mod)
  out <- run_sim(
    model = tmp_mod, # !! filename, not model object
    data = dat,
    variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED")
  )
  expect_equal(dim(out), c(744, 12))
  unlink(tmp_mod)
})

test_that("Errors on invalid variables when update_table = TRUE", {
  local_pharmr.extra_options()
  withr::local_dir(tempdir())
  
  dat <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2),
    DV = c(0, 10, 5),
    AMT = c(100, 0, 0),
    CMT = 1,
    EVID = c(1, 0, 0),
    MDV = c(1, 0, 0)
  )
  mod <- create_model(route = "iv", data = dat, tables = NULL, verbose = FALSE)
  
  expect_error(
    run_sim(
      model = mod, 
      data = dat,
      variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED", "NOPE", "WRONG"),
      update_table = TRUE
    ),
    "NOPE and WRONG are not valid variables"
  )
})
