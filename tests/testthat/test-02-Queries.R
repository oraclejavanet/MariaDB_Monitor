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
  appDbTz <<- "GMT"
  dat <- procToTimeLine(qryProcData())
  colNames <- c("TOT_CONNECTIONS", "TOT_MEMORY", "RUN_CONNECTIONS")
  expect_equal(names(dat), colNames)
  expect_is(dat, "xts")
  expect_is(dat, "zoo")
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

test_that("qryServStatData", {
  checkConnection()
  ## -- Slow Queries ---------------------------------
  slowQryVars <<- c("slow_queries", "long_query_time", "log_slow_queries")
  ## -- Used Connections -- ##
  usedConVars <<- c("max_connections", "max_used_connections", "threads_connected")
  ## -- Worker Threads -- ##
  workerThreadsVars <<- c("threads_cached", "thread_cache_size", "KPI_threadCacheHitrate")
  ## -- Key buffer Size -- ##
  keyBufferSizeVars <<- c("key_read_requests", "key_reads", "key_blocks_used", "key_blocks_unused",
                         "key_cache_block_size", "key_buffer_size", "KPI_keyReadsOnDisk")
  ## -- Query Cache -- ##
  qryCacheVars <<- c("query_cache_size", "query_cache_limit", "query_cache_min_res_unit",
                    "qcache_free_memory", "qcache_total_blocks", "qcache_free_blocks",
                    "qcache_lowmem_prunes", "KPI_qryCacheHitrate")
  ## -- Sort Operations -- ##
  sortOpVars <<- c("sort_merge_passes", "sort_scan", "sort_range", "sort_buffer_size",
                  "read_rnd_buffer_size")
  ## -- Joins -- ##
  joinVars <- c("select_full_join", "select_range_check", "join_buffer_size")
  ## -- Temp Tables -- ##
  tmpTableStatVars <<- c("created_tmp_tables", "created_tmp_disk_tables", "tmp_table_size",
                        "max_heap_table_size", "KPI_tmpTablesOnDisk")
  ## -- Table Locking -- ##
  tblLockingVars <<- c("table_locks_waited", "table_locks_immediate", "concurrent_insert",
                      "low_priority_updates")
  ## -- Table Scans -- ##
  tblScansVars <<- c("com_select", "Select_full_join", "read_rnd_buffer_size", "read_buffer_size")
  ## -- Binlog -- ##
  tblBinlogVars <<- c("binlog_bytes_written", "binlog_cache_disk_use", "binlog_cache_use", "binlog_stmt_cache_disk_use",
                     "binlog_stmt_cache_use", "KPI_binlogCacheNoDiskUse")
  # Memory Used
  memStatVars <<- c("innodb_buffer_pool_bytes_data", "memory_used")
  globalMemVars <<- c("key_buffer_size", "query_cache_size", "innodb_buffer_pool_size",
                     "innodb_additional_mem_pool_size", "innodb_log_buffer_size")
  perConnectionMemVars <<- c("read_buffer_size", "read_rnd_buffer_size", "sort_buffer_size",
                            "join_buffer_size", "binlog_cache_size", "thread_stack", "tmp_table_size")


  ## Vars for transformation for thousend (10000 -> 10 000)  ---------------------------------
  varsTransformThousend <<- c("created_tmp_disk_tables", "created_tmp_tables", "table_locks_immediate",
                             "key_blocks_unused", "slow_queries", "table_locks_waited", "max_used_connections",
                             "threads_connected", "max_connections", "key_blocks_used", "key_read_requests",
                             "key_reads", "qcache_free_blocks", "qcache_lowmem_prunes", "qcache_total_blocks",
                             "query_cache_min_res_unit", "sort_merge_passes", "sort_range", "sort_scan",
                             "com_select", "table_locks_immediate", "table_locks_waited", "binlog_cache_use",
                             "binlog_cache_disk_use", "binlog_stmt_cache_disk_use", "binlog_stmt_cache_use")
  dat <- cleanVarList(qryServStatData())
  colNames <- c("VARIABLE_NAME", "VARIABLE_VALUE")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryLogWrites", {
  checkConnection()
  dat <- cleanLogWrites(qryServStatData())
  colNames <- c("VARIABLE_NAME", "VARIABLE_VALUE", "DATETIME", "VARIABLE_VALUE_SEC")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryTmpDiscTblStmt", {
  checkConnection()
  dat <- qryTmpDiscTblStmt()
  colNames <- c("query", "db", "exec_count", "total_latency", "min_latency",
                "avg_latency", "memory_tmp_tables", "disk_tmp_tables", "avg_tmp_tables_per_query",
                "tmp_tables_to_disk_pct", "first_seen", "last_seen")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryProcData", {
  checkConnection()
  dat <- cleanProcList(qryProcData())
  colNames <- c("ID", "USER", "HOST", "DB", "COMMAND",
                "TIME", "PROGRESS", "STATE", "MEMORY_USED",
                "EXAMINED_ROWS", "INFO")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryUserStat", {
  checkConnection()
  dat <- qryUserStat()
  colNames <- c("user", "statements", "stmt_latency", "avg_latency", "tbl_scans",
                "io_latency", "warn_err", "tmp_tbls", "tmp_disk_tbls",
                "rows_send", "current_con", "tot_con", "unique_hosts")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryBufferReads", {
  checkConnection()
  dat <- bufferReads(qryServStatData())
  colNames <- c("VARIABLE_NAME", "VARIABLE_VALUE", "DATETIME", "VARIABLE_VALUE_SEC")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryErrWarnData", {
  checkConnection()
  dat <- qryErrWarnData()
  colNames <- c("query", "db", "exec_count", "errors", "error_pct",
                "warnings", "warning_pct", "first_seen", "last_seen")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

test_that("qryIdxCardinality", {
  checkConnection()
  dat <- qryIdxCardinality()
  colNames <- c("CARDINALITY", "TABLE_SCHEMA", "TABLE_NAME", "INDEX_NAME", "COLUMN_NAME",
                "NON_UNIQUE")
  expect_equal(names(dat), colNames)
  expect_is(dat, "data.frame")
})

