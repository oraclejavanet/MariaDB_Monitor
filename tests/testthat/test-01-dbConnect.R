context("dbConnect")

noDBcon <- function() {

  file.exists("~/.INWTdbMonitor/cnf.file") |
    if (file.exists("~/.INWTdbMonitor/cnf.file")) {

      tmp <- readLines("~/.INWTdbMonitor/cnf.file")

      nchar(grepLine(tmp, "user=")) == 0 &
        nchar(grepLine(tmp, "password=")) == 0 &
        nchar(grepLine(tmp, "host=")) == 0 &
        nchar(grepLine(tmp, "port=")) == 0
    } else {TRUE}

}

checkDBcon <- function() {
  if (noDBcon()) {
    skip("No database credentials available.")
  }
}

test_that("Database Credentials", {
  checkDBcon()
  expect_true(nchar(grepLine(dbConfig$get(), "user=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "password=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "host=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "port=")) > 0)

})


test_that("Database Connection", {
  checkDBcon()
  tmp <- testConnection(genSQLCred(), logging = FALSE)
  expect_equal(tmp, TRUE)
})


test_that("Grants for performance schema.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`performance..schema`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Grants for information schema.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`information..schema`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Grants for mysql database.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`mysql`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Privileges for some data (e.g. index cardinality).", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*PROCESS.*ON *.* TO", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Privileges for viewing process list.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("PROCESS.*ON *.* TO", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Performance schema exist.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("performance_schema" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Information schema exist.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("information_schema" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("Mysql database exist.", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("mysql" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)
})


test_that("performance Schema is on", {
  checkDBcon()
  tmp <- sendQuery(genSQLCred(), "SHOW VARIABLES LIKE 'performance_schema';") %>%
    data.frame %>%
    select(Value)
  expect_equal(tmp$Value, 'ON')
})
