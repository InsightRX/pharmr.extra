#' Compare fit of two or more NLME fits
#'
#' @param ... fit objects
#' @param return_object logical, if TRUE, return a list of the combined info and parameter tables
#' @export
#'
compare_nlme_fit <- function(..., return_object = FALSE) {
  fits <- list(...)
  ## Unwrap when the caller passed a single argument that is itself a list of
  ## fits (e.g. `compare_nlme_fit(list(fit1, fit2))`). A single fit object
  ## already has `attr(., "info")` set by attach_fit_info(), so we use that
  ## as the marker — this avoids treating an R-list-shaped fit (e.g. from
  ## nlmixr2) as a list of fits.
  if(length(fits) == 1 && is.null(attr(fits[[1]], "info"))) {
    if(length(fits[[1]]) >= 1) {
      fits <- fits[[1]]
    }
  }
  ## First, combine into a list of parsed info. Prefer an explicit
  ## `run_name` attribute (set by compare_nlme_runs() for reloaded fits)
  ## over `attr(., "model")$name`, since the latter dereferences a pharmpy
  ## model object that may be an invalid Python pointer after readRDS.
  fit_info <- purrr::map(fits, function(x) {
    nm <- attr(x, "run_name")
    if(is.null(nm)) {
      nm <- tryCatch(attr(x, "model")$name, error = function(e) NA_character_)
    }
    list(
      info_tab = create_modelfit_info_table(x),
      par_tab = create_modelfit_parameter_table(x),
      name = nm
    )
  })
  ## Then grab the right info and combine columns from different runs
  info_comb <- combine_info_columns(
    fit_info,
    "info_tab",
    label = "Detail"
  )
  par_comb <- combine_info_columns(
    fit_info,
    "par_tab",
    label = "Parameter"
  )
  if(return_object) {
    return(list(
      info_comb = info_comb,
      par_comb = par_comb
    ))
  } else {
    print(
      knitr::kable(info_comb)
    )
    print(
      knitr::kable(par_comb)
    )
  }
}

#' Combine columns with run info into a data.frame
#' and make sure that rows match (e.g. parameters)
#'
#' data.frames in list should have the same column names but can have different
#' row names (e.g. parameter names).
#'
combine_info_columns <- function(
    fit_info,
    table = "info_tab",
    label = "Detail"
) {
  comb <- dplyr::bind_rows(
    purrr::map(fit_info, function(x) {
      run_name <- PKPDsim::ifelse0(x$name, "n/a")
      res <- data.frame(x[[table]][,-1])
      first_label <- names(res)[1]
      res[,1] <- as.character(res[,1])
      cols <- dplyr::bind_rows(
        data.frame(run_name) |> setNames(first_label),
        res
      )
      rownames(cols) <- c("Run id", x[[table]][,1])
      t(cols) |>
        data.frame() # leverage bind_rows to match parameter names and insert NAs. bind_cols cannot do that.
    })
  ) |>
    t() |>
    data.frame() # pivot back again
  comb
}
