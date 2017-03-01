context("bufferTotDat")

test_that("calculating total buffer", {

  dat <- data.frame(
    VARIABLE_NAME = c("tokudb_cachetable_size_current", "tokudb_cache_size",
                      "innodb_buffer_pool_pages_free", "innodb_buffer_pool_pages_total",
                      "KPI_bufPoolSize"),
    VARIABLE_VALUE = c("900", "1000", "50", "1000", "180"))

  dat <- bufferTotDat(dat)

  expect_equal(dat, 1071)
  expect_is(dat, "numeric")

})
