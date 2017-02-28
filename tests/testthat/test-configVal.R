context("configVal")

test_that("threshSlowQryOrange", {

  configVar <- "threshSlowQryOrange"

  expect_is(configVal(configVar), "numeric")

})
