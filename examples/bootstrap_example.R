## Bootstrap workflow example
##
## What is bootstrapping?
## Bootstrapping is a way to assess how reliable your model's parameter
## estimates are. It works by repeatedly resampling your original dataset
## (drawing random subjects with replacement) and re-fitting the model each
## time. This produces a distribution of estimates for each parameter. If a
## parameter's distribution is tight, the estimate is stable; if it's wide,
## there is more uncertainty. The result is a confidence interval for every
## parameter — no formulas or assumptions required.
##
## Bootstrap is a fairly computationally expensive procedure. Commonly it is
## only run on a final model, or on certain key models.
##
## This script demonstrates the full end-to-end bootstrap workflow using
## pharmr.extra:
##   1. Build a NONMEM model with create_model()
##   2. Fit the model with run_nlme()
##   3. Run bootstrap via call_pharmpy_tool()
##   4. Inspect bootstrap results
##   5. Plot parameter distributions with plot_bootstrap()
##   6. Save the plot
##
## Requirements: NONMEM and Pharmpy must be installed and accessible.

library(pharmr)
devtools::load_all()  # or: library(pharmr.extra)

# ── 1. Build model ────────────────────────────────────────────────────────────

dataset <- data.frame(
  ID   = rep(1:20, each = 5),
  TIME = rep(c(0, 1, 4, 8, 24), times = 20),
  AMT  = rep(c(100, 0, 0, 0, 0), times = 20),
  EVID = rep(c(1, 0, 0, 0, 0), times = 20),
  MDV  = rep(c(1, 0, 0, 0, 0), times = 20),
  DV   = c(replicate(20, c(0, round(rnorm(4, mean = c(5, 3, 1.5, 0.5), sd = 0.3), 2)))),
  CMT = 1
)

model <- create_model(
  data    = dataset, tables = c()
)

# ── 2. Fit the model ──────────────────────────────────────────────────────────
results <- run_nlme(id = "run1", model = model, force = TRUE)

# Inspect parameter estimates
print(results)

# Extract original estimates as a named vector for later overlay
orig_estimates <- results$parameter_estimates
print(orig_estimates)

# ── 3. Run bootstrap ──────────────────────────────────────────────────────────

# Use samples = 200 for a quick run; increase to 1000+ for publication-quality
bs_results <- call_pharmpy_tool(
  id      = "bs1",
  model   = model,
  results = results,
  tool    = "bootstrap",
  options = list(samples = 20)
)

# ── 4. Inspect bootstrap results ──────────────────────────────────────────────

# parameter_estimates: data.frame with one row per sample, one column per parameter
pe <- as.data.frame(bs_results$parameter_estimates)
head(pe)

# Quick summary statistics
summary(pe)

# ── 5. Plot parameter distributions ──────────────────────────────────────────

# Basic plot — all parameters, no overlay
p_basic <- plot_bootstrap(bs_results)
p_basic

# With original estimates overlaid (solid vertical line) and 95% CI (dashed)
p_annotated <- plot_bootstrap(
  bs_results,
  original_estimates = orig_estimates,
  ci                 = 0.95
)
p_annotated

# Subset to only structural parameters
p_structural <- plot_bootstrap(
  bs_results,
  original_estimates = orig_estimates,
  parameters         = c("POP_CL", "POP_V")
)
p_structural
