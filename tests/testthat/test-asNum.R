context("asNum")

test_that("is numeric", {
  expect_is(asNum("w"), "numeric")
  expect_equal(asNum("w"), NA_real_)
})
