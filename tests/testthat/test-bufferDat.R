context("bufferDat")

test_that("calculating of percentage", {

  dat <- data.frame(
    VARIABLE_NAME = c("tokudb_cachetable_size_current", "tokudb_cache_size",
                      "innodb_buffer_pool_pages_free", "innodb_buffer_pool_pages_total"),
    VARIABLE_VALUE = c("900", "1000", "50", "1000"))

  dat <- bufferDat(dat)

  expect_equal(dat[1, 2], 0.9)
  expect_equal(dat[1, 3], 0.1)
  expect_equal(dat[2, 2], 0.95)
  expect_equal(dat[2, 3], 0.05)

  expect_is(dat, "data.frame")

})
