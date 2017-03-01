context("helperBufferWrite")

test_that("calc difference worked", {

  datOld <- data.frame(
    VARIABLE_NAME = c("INNODB_LOG_WRITE_REQUESTS", "LOG_WRITES", "LOG_WRITES_OS_MB"),
    VARIABLE_VALUE  = c(100, 20, 10),
    DATETIME = rep('2017-02-27 16:59:17', 3),
    VARIABLE_VALUE_SEC = NA
    )

  datNew <- data.frame(
    VARIABLE_NAME = c("INNODB_LOG_WRITE_REQUESTS", "LOG_WRITES", "LOG_WRITES_OS_MB"),
    VARIABLE_VALUE  = c(120, 40, 80),
    DATETIME = rep('2017-02-27 17:00:17', 3),
    VARIABLE_VALUE_SEC = NA
  )


  dat <- helperBufferWrite(datOld, datNew)

  expect_is(dat, "data.frame")
  expect_is(dat$DATETIME, "POSIXct")
  expect_is(dat$VARIABLE_VALUE, "numeric")
  expect_is(dat$VARIABLE_VALUE_SEC, "numeric")

  expect_equal(dat$VARIABLE_VALUE_SEC[4], 20)
  expect_equal(dat$VARIABLE_VALUE_SEC[5], 20)
  expect_equal(dat$VARIABLE_VALUE_SEC[6], 70)


  appDbTz <<- "GMT"
  datDygraph <- helperDygraphDat(dat)

  expect_is(datDygraph, "xts")
  expect_equal(datDygraph[[2]], "20")
  expect_equal(datDygraph[[4]], "20")
  expect_equal(datDygraph[[6]], "70")

})
