context("dbConfig")

test_that("mutate values in fun worked", {

  tmp <- dbConfig$get()

  expect_equal(length(dbConfig$get()), 6)

  dbConfig$set("host.server:1234")

  expect_equal(dbConfig$get()[5], "host=host.server")
  expect_equal(dbConfig$get()[6], "port=1234")

  dbConfig$init()

  expect_equal(dbConfig$get(), tmp)

})



