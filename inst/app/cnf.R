## --------------------------------------------------------------------------------------------------
## Server Statistics

## -- Slow Queries ---------------------------------
slowQryVars <- c("slow_queries", "long_query_time", "log_slow_queries")
## -- Used Connections -- ##
usedConVars <- c("max_connections", "max_used_connections", "threads_connected")
## -- Worker Threads -- ##
workerThreadsVars <- c("threads_cached", "thread_cache_size", "KPI_threadCacheHitrate")
## -- Key buffer Size -- ##
keyBufferSizeVars <- c("key_read_requests", "key_reads", "key_blocks_used", "key_blocks_unused",
                       "key_cache_block_size", "key_buffer_size", "KPI_keyReadsOnDisk")
## -- Query Cache -- ##
qryCacheVars <- c("query_cache_size", "query_cache_limit", "query_cache_min_res_unit",
                  "qcache_free_memory", "qcache_total_blocks", "qcache_free_blocks",
                  "qcache_lowmem_prunes", "KPI_qryCacheHitrate")
## -- Sort Operations -- ##
sortOpVars <- c("sort_merge_passes", "sort_scan", "sort_range", "sort_buffer_size",
                "read_rnd_buffer_size")
## -- Joins -- ##
joinVars <- c("select_full_join", "select_range_check", "join_buffer_size")
## -- Temp Tables -- ##
tmpTableStatVars <- c("created_tmp_tables", "created_tmp_disk_tables", "tmp_table_size",
                  "max_heap_table_size", "KPI_tmpTablesOnDisk")
## -- Table Locking -- ##
tblLockingVars <- c("table_locks_waited", "table_locks_immediate", "concurrent_insert",
                  "low_priority_updates")
## -- Table Scans -- ##
tblScansVars <- c("com_select", "Select_full_join", "read_rnd_buffer_size", "read_buffer_size")
## -- Binlog -- ##
tblBinlogVars <- c("binlog_bytes_written", "binlog_cache_disk_use", "binlog_cache_use", "binlog_stmt_cache_disk_use",
                   "binlog_stmt_cache_use", "KPI_binlogCacheNoDiskUse")
# Memory Used
memStatVars <- c("innodb_buffer_pool_bytes_data", "memory_used")
globalMemVars <- c("key_buffer_size", "query_cache_size", "innodb_buffer_pool_size",
                  "innodb_additional_mem_pool_size", "innodb_log_buffer_size")
perConnectionMemVars <- c("read_buffer_size", "read_rnd_buffer_size", "sort_buffer_size",
                  "join_buffer_size", "binlog_cache_size", "thread_stack", "tmp_table_size")
# aborted cons
abortedCons <- c("aborted_clients", "aborted_connects")


## Vars for transformation for thousend (10000 -> 10 000)  ---------------------------------
varsTransformThousend <- c("created_tmp_disk_tables", "created_tmp_tables", "table_locks_immediate",
                  "key_blocks_unused", "slow_queries", "table_locks_waited", "max_used_connections",
                  "threads_connected", "max_connections", "key_blocks_used", "key_read_requests",
                  "key_reads", "qcache_free_blocks", "qcache_lowmem_prunes", "qcache_total_blocks",
                  "query_cache_min_res_unit", "sort_merge_passes", "sort_range", "sort_scan",
                  "com_select", "table_locks_immediate", "table_locks_waited", "binlog_cache_use",
                  "binlog_cache_disk_use", "binlog_stmt_cache_disk_use", "binlog_stmt_cache_use")

# Colors ---------------------------------
appGreen  <- "#4CAF50"
appRed    <- "#F44336"
appOrange <- "#FF9800"
appBlue   <- "#2196F3"
appIndego <- "#3F51B5"
appBlGrey <- "#607D8B"
appYellow <- "#FFEB3B"

# database encoding
encodingDB <- "latin1"

# database timezone
appDbTz <- "GMT"
