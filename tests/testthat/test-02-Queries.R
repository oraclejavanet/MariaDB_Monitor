context("Queries")

checkConnection <- function() {
  if (testConnection(genSQLCred()) == FALSE) {
    skip("No database connection available.")
  }
}

test_that("qryEventData", {
  checkConnection()
  dat <- qryEventData()
  colNames <- c("db", "name", "definer", "created", "modified", "last_executed",
                "status", "comment")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryIdxData", {
  checkConnection()
  dat <- qryIdxData()
  colNames <- c("object_schema", "object_name", "index_name", "CARDINALITY")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryTimeLine", {
  checkConnection()
  dat <- qryTimeLine()
  colNames <- c("TOT_CONNECTIONS", "TOT_MEMORY", "RUN_CONNECTIONS")
  expect_equal(names(dat), colNames)
  expect_is(dat, "xts")
  expect_is(dat, "zoo")
})

test_that("qryLogWrites", {
  checkConnection()
  dat <- qryLogWrites()
  colNames <- c("VARIABLE_NAME", "VARIABLE_VALUE", "DATETIME", "VARIABLE_VALUE_SEC")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryIdxNullable", {
  checkConnection()
  dat <- qryIdxNullable()
  colNames <- c("object_schema", "object_name", "index_name", "nullable", "CARDINALITY")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryStmtAnalysis", {
  checkConnection()
  dat <- qryStmtAnalysis()
  colNames <- c("exec_count", "avg_latency", "query", "full_scan", "db",
                "first_seen", "last_seen")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qry95thPercStmt", {
  checkConnection()
  dat <- qry95thPercStmt()
  colNames <- c("exec_count", "avg_latency", "query", "db")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryFullTblScanStmt", {
  checkConnection()
  dat <- qryFullTblScanStmt()
  colNames <- c("exec_count", "db", "query", "total_latency", "no_index_used_count",
                "no_good_index_used_count", "no_index_used_pct", "first_seen", "last_seen")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

