context("compress")

test_that("compress worked", {

  dat <- compress(1024)

  expect_equal(dat, "1.02K")
  expect_is(dat, "character")

})
