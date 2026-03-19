# Plot bootstrap parameter distributions

Creates a faceted density + histogram plot showing the distribution of
parameter estimates across bootstrap samples. Optionally overlays the
original (fitted) parameter estimate and confidence interval lines.

## Usage

``` r
plot_bootstrap(
  bootstrap_results,
  original_estimates = NULL,
  ci = 0.95,
  parameters = NULL
)
```

## Arguments

- bootstrap_results:

  Bootstrap results object returned by
  `call_pharmpy_tool(tool = "bootstrap")`. Must expose a
  `parameter_estimates` attribute (a pandas DataFrame that reticulate
  auto-converts to an R data.frame).

- original_estimates:

  Named numeric vector of original parameter estimates (e.g. from
  [`run_nlme()`](https://insightrx.github.io/pharmr.extra/reference/run_nlme.md)
  results) to overlay as a solid vertical line on each panel. Names must
  match column names of `bootstrap_results$parameter_estimates`.
  Parameters not found are silently ignored. Pass `NULL` (default) to
  suppress.

- ci:

  Numeric confidence level for the displayed credible interval (e.g.
  `0.95` draws the 2.5th and 97.5th percentile as dashed lines). Pass
  `NULL` to suppress CI lines.

- parameters:

  Character vector of parameter names to include. `NULL` (default)
  includes all parameters.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Use
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
to save it.

## Examples

``` r
if (FALSE) { # \dontrun{
# After running bootstrap:
bs <- call_pharmpy_tool(
  id = "run1",
  model = model,
  tool = "bootstrap",
  options = list(samples = 200)
)

# Basic plot
plot_bootstrap(bs)

# With original estimates overlaid
orig <- c(POP_CL = 4.5, POP_V = 32.1, IIV_CL = 0.09)
p <- plot_bootstrap(bs, original_estimates = orig, ci = 0.95)
ggplot2::ggsave("bootstrap_params.png", p, width = 10, height = 8)
} # }
```
