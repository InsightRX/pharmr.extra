test_that("basic search space creation works with default parameters", {
  out <- create_pkmodel_search_space()
  
  expect_type(out, "character")
  expect_equal(
    out,
    "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])"
  )
})

test_that("single absorption option works", {
  out <- create_pkmodel_search_space(absorption = "FO")
  
  expect_equal(out, "ABSORPTION([FO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("multiple absorption options work", {
  out <- create_pkmodel_search_space(absorption = c("FO", "ZO", "INST"))
  
  expect_equal(out, "ABSORPTION([FO,ZO,INST]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("all valid absorption options work", {
  out <- create_pkmodel_search_space(absorption = c("INST", "FO", "ZO", "SEQ-ZO-FO"))
  
  expect_equal(out, "ABSORPTION([INST,FO,ZO,SEQ-ZO-FO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("single elimination option works", {
  out <- create_pkmodel_search_space(elimination = "FO")
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO]); PERIPHERALS([0,1])")
})

test_that("multiple elimination options work", {
  out <- create_pkmodel_search_space(elimination = c("FO", "MM", "ZO"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM,ZO]); PERIPHERALS([0,1])")
})

test_that("all valid elimination options work", {
  out <- create_pkmodel_search_space(elimination = c("FO", "ZO", "MM", "MIX-FO-MM"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,ZO,MM,MIX-FO-MM]); PERIPHERALS([0,1])")
})

test_that("numeric peripherals values work", {
  out <- create_pkmodel_search_space(peripherals = 0)
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0])")
})

test_that("multiple numeric peripherals values work", {
  out <- create_pkmodel_search_space(peripherals = c(0, 1, 2))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1,2])")
})

test_that("string peripherals values work", {
  out <- create_pkmodel_search_space(peripherals = c("DRUG", "MET"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([DRUG,MET])")
})



test_that("numeric transits values work", {
  out <- create_pkmodel_search_space(transits = 0)
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("multiple numeric transits values work", {
  out <- create_pkmodel_search_space(transits = c(0, 1, 2, 3))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,2,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("string transits values work", {
  out <- create_pkmodel_search_space(transits = c("DEPOT", "NODEPOT"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([DEPOT,NODEPOT]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})



test_that("single lagtime option works", {
  out <- create_pkmodel_search_space(lagtime = "OFF")
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("both lagtime options work", {
  out <- create_pkmodel_search_space(lagtime = c("OFF", "ON"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("custom combination of parameters works", {
  out <- create_pkmodel_search_space(
    absorption = "FO",
    elimination = "MM",
    peripherals = 1,
    transits = 0,
    lagtime = "ON"
  )
  
  expect_equal(out, "ABSORPTION([FO]); TRANSITS([0]); LAGTIME([ON]); ELIMINATION([MM]); PERIPHERALS([1])")
})

test_that("mixed numeric and string peripherals values work", {
  # TODO: currently fails. Decide if this behaviour should be supported.
  skip()
  out <- create_pkmodel_search_space(peripherals = c(0, "DRUG"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,DRUG])")
})

test_that("mixed numeric and string transits values work", {
  # TODO: currently fails. Decide if this behaviour should be supported.
  skip()
  out <- create_pkmodel_search_space(transits = c(0, "DEPOT"))
  
  expect_equal(out, "ABSORPTION([FO,ZO]); TRANSITS([0,DEPOT]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("complex example with all parameters works", {
  # TODO: currently fails because mixed numeric and strings are not supported.
  skip()
  out <- create_pkmodel_search_space(
    absorption = c("FO", "ZO", "INST"),
    elimination = c("FO", "MM", "ZO"),
    peripherals = c(0, 1, 2, "DRUG"),
    transits = c(0, 1, 3, "DEPOT"),
    lagtime = c("OFF", "ON")
  )
  
  expect_equal(
    out,
    "ABSORPTION([FO,ZO,INST]); TRANSITS([0,1,3,DEPOT]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM,ZO]); PERIPHERALS([0,1,2,DRUG])"
  )
})

test_that("empty vector for a parameter is handled", {
  out <- create_pkmodel_search_space(absorption = character(0))
  
  # Empty vector should be skipped (length == 0)
  expect_equal(out, "TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("NULL parameter is handled", {
  out <- create_pkmodel_search_space(absorption = NULL)
  
  # NULL should be skipped
  expect_equal(out, "TRANSITS([0,1,3]); LAGTIME([OFF,ON]); ELIMINATION([FO,MM]); PERIPHERALS([0,1])")
})

test_that("error when invalid absorption option provided", {
  expect_error(
    create_pkmodel_search_space(absorption = "INVALID"),
    "Some options not recongized"
  )
})

test_that("error when invalid elimination option provided", {
  expect_error(
    create_pkmodel_search_space(elimination = "INVALID"),
    "Some options not recongized"
  )
})

test_that("error when invalid peripherals option provided", {
  expect_error(
    create_pkmodel_search_space(peripherals = "INVALID"),
    "Some options not recongized"
  )
})

test_that("error when invalid transits option provided", {
  expect_error(
    create_pkmodel_search_space(transits = "INVALID"),
    "Some options not recongized"
  )
})

test_that("error when invalid lagtime option provided", {
  expect_error(
    create_pkmodel_search_space(lagtime = "INVALID"),
    "Some options not recongized"
  )
})

test_that("error when one of multiple absorption options is invalid", {
  expect_error(
    create_pkmodel_search_space(absorption = c("FO", "INVALID")),
    "Some options not recongized"
  )
})
