context("appDataTable")

test_that("Is a datatable htmlwidget", {

  dat <- data.frame(
    VARIABLE_NAME = c("var1", "var2"),
    VARIABLE_VALUE = c("1", "2"))

  expect_is(appDataTable(dat), "datatables")
  expect_is(appDataTable(dat), "htmlwidget")

})
