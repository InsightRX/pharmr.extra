# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

skip_on_ci()

test_that("Basic simulation works (using `model` argument, not `fit`)", {
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

test_that("Basic simulation works (using model code specified to )", {
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
  out <- run_sim(
    model = model_code, # !! code, not model object
    data = dat,
    variables = c("ID", "TIME", "DV", "EVID", "CIPREDI", "PRED")
  )
  expect_equal(dim(out), c(744, 12))
})
