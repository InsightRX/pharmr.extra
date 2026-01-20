# get_pharmpy_conf() ----------------------------------------------------------
test_that("returns valid config list when config file exists", {
  # Create temporary directory and config file:
  tmp_dir <- withr::local_tempdir()
  config_file <- file.path(tmp_dir, "pharmpy.conf")
  
  # Create a valid ini config file:
  ini_content <- c(
    "[database]",
    "path = /path/to/database",
    "name = test_db",
    "",
    "[tools]",
    "run_timeout = 3600",
    "auto_yes = True"
  )
  writeLines(ini_content, config_file)
  
  # Mock get_config_path to return our temp file:
  local_mocked_bindings(get_config_path = function() config_file)
  out <- get_pharmpy_conf()
  
  expect_type(out, "list")
  expect_named(out, c("database", "tools"))
  expect_equal(out$database$path, "/path/to/database")
  expect_equal(out$database$name, "test_db")
  expect_equal(out$tools$run_timeout, "3600")
  expect_equal(out$tools$auto_yes, "True")
})

test_that("aborts when config path is NULL", {
  # Mock pharmr::get_config_path to return NULL
  local_mocked_bindings(
    get_config_path = function() NULL
  )
  
  expect_error(
    get_pharmpy_conf(),
    "Cannot find Pharmpy configuration file"
  )
})

test_that("aborts when config file does not exist", {
  tmp_dir <- withr::local_tempdir()
  non_existent_file <- file.path(tmp_dir, "does_not_exist.conf")
  
  local_mocked_bindings(
    get_config_path = function() non_existent_file
  )
  
  expect_error(
    get_pharmpy_conf(),
    "Cannot find Pharmpy configuration file"
  )
})

# read_ini() ------------------------------------------------------------------
test_that("handles empty sections", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  ini_content <- c(
    "[empty_section]",
    "",
    "[section_with_values]",
    "key = value"
  )
  writeLines(ini_content, ini_file)
  
  out <- read_ini(ini_file)
  
  expect_type(out, "list")
  expect_named(out, c("empty_section", "section_with_values"))
  expect_length(out$empty_section, 0)
  expect_equal(out$section_with_values$key, "value")
})

test_that("handles blank lines and whitespace", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  ini_content <- c(
    "",
    "[section1]",
    "",
    "key1 = value1",
    "",
    "key2 = value2",
    ""
  )
  writeLines(ini_content, ini_file)
  
  out <- read_ini(ini_file)
  
  expect_type(out, "list")
  expect_named(out, "section1")
  expect_equal(out$section1$key1, "value1")
  expect_equal(out$section1$key2, "value2")
})

test_that("handles key-value pairs with whitespace around equals", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  ini_content <- c(
    "[section]",
    "key1=value1",
    "key2 = value2",
    "key3  =  value3",
    "key4\t=\tvalue4"
  )
  writeLines(ini_content, ini_file)
  
  out <- read_ini(ini_file)
  
  expect_equal(out$section$key1, "value1")
  expect_equal(out$section$key2, "value2")
  expect_equal(out$section$key3, "value3")
  expect_equal(out$section$key4, "value4")
})

test_that("handles multiple sections", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  ini_content <- c(
    "[section1]",
    "key1 = value1",
    "[section2]",
    "key2 = value2",
    "[section3]",
    "key3 = value3"
  )
  writeLines(ini_content, ini_file)
  
  out <- read_ini(ini_file)
  
  expect_length(out, 3)
  expect_named(out, c("section1", "section2", "section3"))
  expect_equal(out$section1$key1, "value1")
  expect_equal(out$section2$key2, "value2")
  expect_equal(out$section3$key3, "value3")
})

test_that("read_ini handles lines without equals sign", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  ini_content <- c(
    "[section]",
    "key1 = value1",
    "this is not a key value pair",
    "key2 = value2"
  )
  writeLines(ini_content, ini_file)
  
  out <- read_ini(ini_file)
  
  # Should only parse lines with equals sign:
  expect_equal(length(out$section), 2)
  expect_equal(out$section$key1, "value1")
  expect_equal(out$section$key2, "value2")
})

test_that("errors when key-value pair appears before section header", {
  tmp_dir <- withr::local_tempdir()
  ini_file <- file.path(tmp_dir, "test.ini")
  
  # Key-value pair before any section (malformed INI):
  ini_content <- c(
    "key = value",
    "[section]",
    "key2 = value2"
  )
  writeLines(ini_content, ini_file)
  
  # This should error because section variable is not defined when processing
  # first key-value pair This is expected behaviour: INI files should have
  # section headers before key-value pairs
  expect_error(
    read_ini(ini_file)
  )
})

# extract() -------------------------------------------------------------------
test_that("extract extracts match from regex correctly", {
  test_string <- "[section_name]"
  out <- extract("^\\[(.*)\\]$", test_string)
  expect_equal(out, "section_name")
  
  test_string2 <- "key = value"
  out2 <- extract("^.*=\\s*(.*)$", test_string2)
  expect_equal(out2, "value")
})
