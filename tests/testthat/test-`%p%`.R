context("`%p%`")

## TODO: Rename context
## TODO: Add more tests

test_that("paste without space", {

  char0 <- "paste" %p0% "without" %p0% "space"
  char1 <- "paste" %p% "with" %p% "space"

  expect_equal(char0, "pastewithoutspace")
  expect_equal(char1, "paste with space")

  expect_is(char0, "character")
  expect_is(char1, "character")

})
