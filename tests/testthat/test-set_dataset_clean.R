clean_model_code <- paste0(
  "$PROBLEM Test\n$INPUT ID TIME DV AMT EVID MDV WT\n$DATA data.csv IGNORE=@\n",
  "$SUBROUTINES ADVAN1 TRANS2\n$PK\nCL=THETA(1)*(WT/70)\nV=THETA(2)\nS1=V\n",
  "$ERROR\nY=F+EPS(1)\n$THETA (0,10)\n$THETA (0,50)\n$SIGMA 0.1\n$EST METHOD=1\n"
)

clean_data <- function() {
  data.frame(
    ID = rep(1:2, each = 2), TIME = c(0, 1, 0, 1), DV = c(0, 1, 0, 2),
    AMT = c(100, 0, 100, 0), EVID = c(1, 0, 1, 0), MDV = c(1, 0, 1, 0),
    WT = c(70, 70, 80, 80)
  )
}

test_that("set_dataset_clean sets a data.frame and reorders columns to $INPUT", {
  mod <- pharmr::read_model_from_string(clean_model_code)
  df <- clean_data()[, c("WT", "ID", "TIME", "DV", "AMT", "EVID", "MDV")]
  res <- set_dataset_clean(mod, df)
  expect_equal(names(res$dataset), c("ID", "TIME", "DV", "AMT", "EVID", "MDV", "WT"))
  expect_equal(as.numeric(res$dataset$WT), c(70, 70, 80, 80))
  ## $DATA points at the new dataset file, options kept
  data_line <- grep("^\\$DATA", strsplit(res$code, "\n")[[1]], value = TRUE)
  expect_false(grepl("data.csv IGNORE", data_line, fixed = TRUE))
  expect_match(data_line, "IGNORE=@")
  ## rest of the code is untouched
  expect_match(res$code, "CL=THETA(1)*(WT/70)", fixed = TRUE)
})

test_that("set_dataset_clean reads a dataset from a CSV path", {
  mod <- pharmr::read_model_from_string(clean_model_code)
  f <- withr::local_tempfile(fileext = ".csv")
  write.csv(clean_data(), f, row.names = FALSE, quote = FALSE)
  res <- set_dataset_clean(mod, f)
  expect_equal(nrow(res$dataset), 4)
  expect_equal(as.numeric(res$dataset$DV), c(0, 1, 0, 2))
})
