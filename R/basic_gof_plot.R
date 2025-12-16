#' Create basic goodness of fit plots based on nlmixr2 fit
#' 
#' @param fit fit object from nlmixr2
#' @param path if not NULL, path to save plot as file (file extension determines 
#' the file format automatically).
#' 
#' @return plotly object. If `path` is specified, will not return anything, but
#'   plotly file will be saved at the requested path.
#' 
#' @export
#' 
basic_gof_plot <- function(
  fit,
  path = NULL
) {
  if(! "EVID" %in% names(fit)) {
    fit$EVID <- 0
  }
  gof_data <- fit %>% 
    data.frame() %>% # nlmixr2 object is a custom class
    dplyr::select("ID", "TIME", "EVID", "PRED", "IPRED", "DV", "CWRES") %>%
    dplyr::filter(.data$EVID == 0)
  p1 <- gof_data %>%
    tidyr::pivot_longer(cols = c("PRED", "IPRED")) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value, y = .data$DV)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::facet_wrap(~name) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
      ggplot2::xlab("") +
      ggplot2::ylab("observed")
  p1 <- plotly::ggplotly(p1)
  p2 <- gof_data %>%
    tidyr::pivot_longer(cols = c("TIME", "PRED")) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value, y = .data$CWRES)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::facet_wrap(~name, scales = "free") +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
      ggplot2::xlab("") +
      ggplot2::ylab("CWRES")
  p2 <- plotly::ggplotly(p2)
  p_comb <- plotly::subplot(
    p1,
    p2,
    nrows = 2, 
    margin = 0.03,
    titleX = TRUE,
    titleY = TRUE
  )
  if(!is.null(path)) {
    htmlwidgets::saveWidget(p_comb, file = path)
  } else {
    return(p_comb)
  }
}
