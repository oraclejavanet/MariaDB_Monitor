context("03-cnfFile")

checkFile <- function() {
  if (!file.exists("~/.INWTdbMonitor/cnf.file")) {
    skip("No cnf-file available.")
  }
}

test_that("initDbServer", {
  checkFile()
  dat <- initDbServer()
  expect_is(dat, "character")
  expect_true(grep(":", dat) == 1)
})

test_that("mutateDbConfig", {
  checkFile()
  dat <- mutateDbConfig()$get()
  expect_length(dat, 6)
  expect_is(dat, "character")
})


