#' Plot bootstrap parameter distributions
#'
#' Creates a faceted density + histogram plot showing the distribution of
#' parameter estimates across bootstrap samples. Optionally overlays the
#' original (fitted) parameter estimate and confidence interval lines.
#'
#' @param bootstrap_results Bootstrap results object returned by
#'   `call_pharmpy_tool(tool = "bootstrap")`. Must expose a
#'   `parameter_estimates` attribute (a pandas DataFrame that reticulate
#'   auto-converts to an R data.frame).
#' @param original_estimates Named numeric vector of original parameter
#'   estimates (e.g. from `run_nlme()` results) to overlay as a solid vertical
#'   line on each panel. Names must match column names of
#'   `bootstrap_results$parameter_estimates`. Parameters not found are silently
#'   ignored. Pass `NULL` (default) to suppress.
#' @param ci Numeric confidence level for the displayed credible interval
#'   (e.g. `0.95` draws the 2.5th and 97.5th percentile as dashed lines). Pass
#'   `NULL` to suppress CI lines.
#' @param parameters Character vector of parameter names to include. `NULL`
#'   (default) includes all parameters.
#'
#' @return A [ggplot2::ggplot()] object. Use [ggplot2::ggsave()] to save it.
#'
#' @examples
#' \dontrun{
#' # After running bootstrap:
#' bs <- call_pharmpy_tool(
#'   id = "run1",
#'   model = model,
#'   tool = "bootstrap",
#'   options = list(samples = 200)
#' )
#'
#' # Basic plot
#' plot_bootstrap(bs)
#'
#' # With original estimates overlaid
#' orig <- c(POP_CL = 4.5, POP_V = 32.1, IIV_CL = 0.09)
#' p <- plot_bootstrap(bs, original_estimates = orig, ci = 0.95)
#' ggplot2::ggsave("bootstrap_params.png", p, width = 10, height = 8)
#' }
#'
#' @export
plot_bootstrap <- function(
  bootstrap_results,
  original_estimates = NULL,
  ci = 0.95,
  parameters = NULL
) {
  pe <- tryCatch(
    as.data.frame(bootstrap_results$parameter_estimates),
    error = function(e) {
      cli::cli_abort(
        "Could not extract `parameter_estimates` from bootstrap results: {e$message}"
      )
    }
  )

  if (nrow(pe) == 0) {
    cli::cli_abort("`parameter_estimates` contains no rows.")
  }

  ## Subset parameters if requested
  if (!is.null(parameters)) {
    missing_params <- setdiff(parameters, names(pe))
    if (length(missing_params) > 0) {
      cli::cli_warn(
        "Parameters not found in bootstrap results and will be skipped: {paste(missing_params, collapse = ', ')}"
      )
    }
    keep <- intersect(parameters, names(pe))
    if (length(keep) == 0) {
      cli::cli_abort("None of the requested `parameters` were found in bootstrap results.")
    }
    pe <- pe[, keep, drop = FALSE]
  }

  ## Pivot to long format
  pe_long <- tidyr::pivot_longer(
    pe,
    cols = tidyr::everything(),
    names_to = "parameter",
    values_to = "value"
  )

  p <- ggplot2::ggplot(pe_long, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 30,
      fill = "steelblue",
      alpha = 0.6,
      color = NA
    ) +
    ggplot2::geom_density(color = "steelblue", linewidth = 0.8) +
    ggplot2::facet_wrap(~parameter, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Parameter value",
      y = "Density",
      title = "Bootstrap parameter distributions"
    )

  ## Overlay original estimates
  if (!is.null(original_estimates)) {
    orig_df <- data.frame(
      parameter = names(original_estimates),
      value = unname(original_estimates),
      stringsAsFactors = FALSE
    )
    orig_df <- orig_df[orig_df$parameter %in% names(pe), , drop = FALSE]
    if (nrow(orig_df) > 0) {
      p <- p + ggplot2::geom_vline(
        data = orig_df,
        ggplot2::aes(xintercept = .data$value),
        color = "black",
        linewidth = 0.9
      )
    }
  }

  ## Overlay CI lines
  if (!is.null(ci)) {
    alpha_half <- (1 - ci) / 2
    ci_long <- do.call(rbind, lapply(names(pe), function(param) {
      vals <- pe[[param]]
      data.frame(
        parameter = param,
        value = stats::quantile(vals, probs = c(alpha_half, 1 - alpha_half), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    p <- p + ggplot2::geom_vline(
      data = ci_long,
      ggplot2::aes(xintercept = .data$value),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.6
    )
  }

  p
}
