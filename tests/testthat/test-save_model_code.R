# TODO: add tests for checking the code block portion of the output after the
# function output has been fixed.

test_that("saves model code to markdown file in correct format", {
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
})
