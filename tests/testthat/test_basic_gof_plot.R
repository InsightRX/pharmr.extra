test_that("basic_gof_plot works", {
  
  # skip() # TODO: figure out how to make this pass in GitHub Actions
  skip_on_os(os = "mac", arch = "aarch64")

  # # sample model from nlmixr2 docs
  # model <- function() {
  #   ini({
  #     tka <- log(1.57); label("Ka")
  #     tcl <- log(2.72); label("Cl")
  #     tv <- log(31.5); label("V")
  #     eta.ka ~ 0.6
  #     eta.cl ~ 0.3
  #     eta.v ~ 0.1
  #     add.sd <- 0.7
  #   })
  #   model({
  #     ka <- exp(tka + eta.ka)
  #     cl <- exp(tcl + eta.cl)
  #     v <- exp(tv + eta.v)
  #     d/dt(depot) <- -ka * depot
  #     d/dt(center) <- ka * depot - cl / v * center
  #     cp <- center / v
  #     cp ~ add(add.sd)
  #   })
  # }
  # suppressMessages(
  #   fit <- nlmixr2::nlmixr2(
  #     model,
  #     nlmixr2data::theo_sd,
  #     est = "saem",
  #     nlmixr2::saemControl(print = 0),
  #     nlmixr2::tableControl(cwres = TRUE, npde = TRUE)
  #   )
  # )
  
  fit <- readRDS(file = system.file("testdata/nlmixr2/fit_example.rds", package = "pharmaair"))
  res <- basic_gof_plot(fit)
  expect_s3_class(res, "plotly")

  tmpfile <- withr::local_file(file.path(tempdir(), "gof.html"))
  basic_gof_plot(fit, path = tmpfile)
  html <- readLines(tmpfile)
  expect_equal(html[1], "<!DOCTYPE html>")
})
