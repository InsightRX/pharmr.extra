## Bootstrap workflow example
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
  DV   = c(replicate(20, c(0, round(rnorm(4, mean = c(5, 3, 1.5, 0.5), sd = 0.3), 2))))
)

model <- create_model(
  dataset = dataset,
  id      = "run1",
  advan   = "ADVAN1",
  trans   = "TRANS2"
)

# ── 2. Fit the model ──────────────────────────────────────────────────────────

results <- run_nlme(id = "run1", model = model)

# Inspect parameter estimates
print(results)

# Extract original estimates as a named vector for later overlay
orig_estimates <- setNames(
  results$parameter_estimates$estimates,
  results$parameter_estimates$parameter
)
print(orig_estimates)

# ── 3. Run bootstrap ──────────────────────────────────────────────────────────

# Use samples = 200 for a quick run; increase to 1000+ for publication-quality
bs_results <- call_pharmpy_tool(
  id      = "run1",
  model   = model,
  results = results,
  tool    = "bootstrap",
  options = list(samples = 200)
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
print(p_basic)

# With original estimates overlaid (solid vertical line) and 95% CI (dashed)
p_annotated <- plot_bootstrap(
  bs_results,
  original_estimates = orig_estimates,
  ci                 = 0.95
)
print(p_annotated)

# Subset to only structural parameters
p_structural <- plot_bootstrap(
  bs_results,
  original_estimates = orig_estimates,
  parameters         = c("POP_CL", "POP_V")
)
print(p_structural)

# ── 6. Save the plot ──────────────────────────────────────────────────────────

ggplot2::ggsave(
  filename = "bootstrap_parameters.png",
  plot     = p_annotated,
  width    = 10,
  height   = 8,
  dpi      = 150
)

cat("Bootstrap example complete. Plot saved to bootstrap_parameters.png\n")
