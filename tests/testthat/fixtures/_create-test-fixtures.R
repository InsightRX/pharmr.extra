# Code to create test fixtures ------------------------------------------------
library(tidyverse)
load_all()

# Read the dataset
nm_dat <- read_csv('data.csv')
nm_dat <- nm_dat %>%
  filter(!(DV == 0 & EVID == 0) & TIME < 12)

# Read the model code from file and combine into single string
model_code <- paste(readLines("run.mod", warn = FALSE), collapse = "\n")
model <- pharmr::read_model_from_string(model_code)
export_pharmpy_model(model, "model.rds")

# Run model fit
fit <- run_nlme(
  model = model_code,
  data = nm_dat,
  id = "20:17:10_2026_01_20_poppk_run",
  estimation_method = "foce",
  full_tables = TRUE,
  save_fit = FALSE,
  force = TRUE
)
export_pharmpy_results(fit, "fit.rds")
