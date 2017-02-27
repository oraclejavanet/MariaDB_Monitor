context("dbConnect")

test_that("No Database Credentials entered.", {

  expect_true(nchar(grepLine(dbConfig$get(), "user=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "password=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "host=")) > 0)
  expect_true(nchar(grepLine(dbConfig$get(), "port=")) > 0)

})


test_that("Can't connect to Database Server.", {
  tmp <- testConnection(genSQLCred(), logging = FALSE)
  expect_equal(tmp, TRUE)

})


test_that("Missing grants for performance schema.", {
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`performance..schema`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Missing grants for information schema.", {
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`information..schema`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Missing grants for mysql database.", {
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*`mysql`", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Insufficient privileges. Some data may be invisible (e.g. index cardinality).", {
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("SELECT.*PROCESS.*ON *.* TO", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Insufficient privileges. Process List won't be available.", {
  tmp <- sendQuery(genSQLCred(), "SHOW GRANTS;") %>%
    data.frame %>%
    filter(grepl("PROCESS.*ON *.* TO", .[, 1])) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Performance schema is missing.", {
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("performance_schema" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Information schema is missing.", {
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("information_schema" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("Mysql database is missing.", {
  tmp <- sendQuery(genSQLCred(), "show databases;") %>%
    data.frame %>%
    filter("mysql" == .[, 1]) %>%
    summarise( missing = ifelse(n() > 0, FALSE, TRUE))
  expect_equal(tmp$missing, FALSE)

})


test_that("performance Schema is not on", {
  tmp <- sendQuery(genSQLCred(), "SHOW VARIABLES LIKE 'performance_schema';") %>%
    data.frame %>%
    select(Value)
  expect_equal(tmp$Value, 'ON')

})
