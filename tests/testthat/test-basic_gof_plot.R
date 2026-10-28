test_that("basic_gof_plot works on a real nlmixr2 fit object", {
  ## Fits a small model live rather than reading a stored fit: serialized
  ## nlmixr2 fits are tied to the nlmixr2 version that wrote them, and the
  ## fixture this test used to read lived in another (private) package.
  skip_if_not_installed("nlmixr2est")
  skip_if_not_installed("nlmixr2data")

  model <- function() {
    ini({
      tka <- log(1.57)
      tcl <- log(2.72)
      tv <- log(31.5)
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }
  ## Zero outer iterations: only the fit object's structure matters here.
  fit <- suppressMessages(suppressWarnings(nlmixr2est::nlmixr2(
    model,
    nlmixr2data::theo_sd,
    est = "focei",
    control = nlmixr2est::foceiControl(
      print = 0, maxOuterIterations = 0, covMethod = ""
    ),
    table = nlmixr2est::tableControl(cwres = TRUE)
  )))
  expect_s3_class(fit, "nlmixr2FitData")
  res <- basic_gof_plot(fit)
  expect_s3_class(res, "plotly")
})

## Synthetic stand-in for an nlmixr2 fit: basic_gof_plot() only needs the
## table columns, so a data.frame is enough and no fitted model is required.
mock_gof_fit <- function(evid = TRUE) {
  set.seed(1)
  n <- 20
  pred <- seq(1, 20, length.out = n)
  fit <- data.frame(
    ID = rep(1:2, each = n / 2),
    TIME = rep(seq(0.5, 10, length.out = n / 2), 2),
    PRED = pred,
    IPRED = pred * 1.05,
    DV = pred + rnorm(n, sd = 0.5),
    CWRES = rnorm(n)
  )
  if (evid) {
    fit$EVID <- 0
    fit <- rbind(fit, transform(fit[1, ], EVID = 1, DV = 999))
  }
  fit
}

test_that("basic_gof_plot returns a combined plotly object from a fit table", {
  res <- basic_gof_plot(mock_gof_fit())
  expect_s3_class(res, "plotly")
})

test_that("basic_gof_plot handles fit tables without an EVID column", {
  res <- basic_gof_plot(mock_gof_fit(evid = FALSE))
  expect_s3_class(res, "plotly")
})

test_that("basic_gof_plot excludes dosing records", {
  captured <- NULL
  real_ggplotly <- plotly::ggplotly
  local_mocked_bindings(
    ggplotly = function(p, ...) {
      captured <<- c(captured, list(p$data))
      real_ggplotly(p, ...)
    },
    .package = "plotly"
  )
  basic_gof_plot(mock_gof_fit())
  expect_length(captured, 2)
  for (d in captured) {
    expect_true(all(d$EVID == 0))
    expect_false(999 %in% d$DV)
  }
})

test_that("basic_gof_plot saves to file when path is given", {
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  tmpfile <- withr::local_tempfile(fileext = ".html")
  res <- basic_gof_plot(mock_gof_fit(), path = tmpfile)
  expect_null(res)
  expect_true(file.exists(tmpfile))
  expect_equal(readLines(tmpfile, n = 1), "<!DOCTYPE html>")
})
