context("appPieChart")


test_that("multiplication works", {

  lab <- c("good", "bad")
  val <- c(1, -1)

  expect_is(appPieChart(lab, val), "gvis")
  expect_is(appPieChart(lab, val), "list")

})
