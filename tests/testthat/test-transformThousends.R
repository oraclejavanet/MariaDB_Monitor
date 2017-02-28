context("transformThousends")

test_that("transform thousends worked", {


  dat <- data.frame(
    VARIABLE_NAME = c("var1", "var2"),
    VARIABLE_VALUE = c(1500, 20000))

  varsTransformThousend <- "var1"

  dat <- transformThousends(dat)

  expect_equal(dat$VARIABLE_VALUE[1], "1 500")
  expect_equal(dat$VARIABLE_VALUE[2], "20000")
  expect_is(dat$VARIABLE_VALUE[1], "character")
  expect_is(dat, "data.frame")

})





