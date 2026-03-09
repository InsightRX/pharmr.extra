test_that("load_data_wrapper returns NULL when data is NULL", {
  expect_null(load_data_wrapper(NULL))
})

test_that("load_data_wrapper returns data.frame unchanged when given a data.frame", {
  df <- data.frame(ID = 1:3, DV = c(10, 5, 2))
  result <- load_data_wrapper(df)
  expect_identical(result, df)
})

test_that("load_data_wrapper reads a CSV file and returns a data.frame", {
  df <- data.frame(ID = 1:3, TIME = c(0, 1, 2), DV = c(10, 5, 2))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- load_data_wrapper(tmp)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("ID", "TIME", "DV"))
  expect_equal(nrow(result), 3)
  expect_equal(result$DV, c(10, 5, 2))
})

test_that("load_data_wrapper errors when file does not exist", {
  expect_error(
    load_data_wrapper("/nonexistent/path/data.csv"),
    "`data` file does not exist"
  )
})
