context("serverVal")


test_that("extract server values worked", {

  dat <- data.frame(
    VARIABLE_NAME = c("var1", "var2"),
    VARIABLE_VALUE = c("1", "2"))

  expect_equal(serverVal(dat, "var2"), "2")
  expect_equal(serverValNum(dat, "var2"), 2)

  expect_is(serverValNum(dat, "var2"), "numeric")

})
