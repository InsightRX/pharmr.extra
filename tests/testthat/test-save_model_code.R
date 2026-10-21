test_that("saves model code to markdown file in correct format", {
  skip_on_covr() # FIXME: fails on test_coverage()
  model <- nlmixr2_pk_1cmt_oral_linear()
  tmp_file <- withr::local_tempfile(fileext = ".md")
  save_model_code(model, tmp_file)
  expect_true(file.exists(tmp_file))
  
  # Verify content:
  content <- readLines(tmp_file)
  expect_equal(content[1], "## Model code")
  expect_equal(content[2], "")
  expect_equal(content[3], "```")
  expect_true(content[length(content)] == "```")
  
  full_content <- paste(readLines(tmp_file), collapse = "\n")
  expect_true(startsWith(full_content, "## Model code"))
  expect_true(grepl("```\n.*\n```", full_content))
  ## a single fenced block holding the model function
  expect_equal(sum(content == "```"), 2)
  expect_true(any(grepl("ini\\(", content)))
  expect_true(any(grepl("model\\(", content)))
})

test_that("wraps character model code in a fenced markdown block", {
  tmp_file <- withr::local_tempfile(fileext = ".md")
  save_model_code(c("$PROBLEM test", "$PK", "CL = THETA(1)"), tmp_file)
  expect_equal(
    readLines(tmp_file),
    c("## Model code", "", "```", "$PROBLEM test", "$PK", "CL = THETA(1)", "```")
  )
})
