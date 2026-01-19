test_that("nlmixr identified correctly", {
  local_pharmr.extra_options()
  mod <- create_model("iv", tool = "nlmixr")
  expect_equal(get_tool_from_model(mod), "nlmixr")
})

test_that("nonmem identified correctly", {
  local_pharmr.extra_options()
  mod <- create_model("iv", tool = "nonmem")
  expect_equal(get_tool_from_model(mod), "nonmem")
})

test_that("invalid inputs fail", {
  # TODO: once input validation is better on this function, we should write tests
  # to ensure invalid inputs fail in some way.
  skip()
  get_tool_from_model(1)
  get_tool_from_model("abc")
  # etc.
})
