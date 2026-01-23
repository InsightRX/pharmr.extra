# TODO: add tests. Tests need to add skip function if nonmem isn't installed.

# mod <- pharmr::load_example_model("pheno")
# pharmr::load_dataset(mod)
# dat <- mod$dataset |> 
#   as.data.frame() |> 
#   dplyr::mutate(
#     EVID = ifelse(AMT == 0, 0, 1), 
#     MDV = ifelse(DV == 0, 1, 0),
#     CMT = 1
#   )
# fit <- pharmr::load_example_modelfit_results("pheno")
# out <- run_sim(fit = fit, model = mod, data = dat)
