context("configVal")

test_that("threshhold is numeric", {

  configVar <- "threshSlowQryOrange"

  expect_is(configVal(configVar), "numeric")

})
