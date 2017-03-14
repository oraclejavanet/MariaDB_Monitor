#' General Server-Status
#'
#' Function to query mariadb global (status) variables.
#'
#' @export
qryServStatData <- function() {
  # Load Status Variables
  innoDBstat <- queryDB("Select * from information_schema.GLOBAL_STATUS
                         UNION ALL
                         Select * from information_schema.GLOBAL_VARIABLES;") %>%
    mutate(VARIABLE_NAME = tolower(VARIABLE_NAME))

  # Insert new Variables in Dataset
  innoDBstat <- innoDBstat %>%

    # Status
    rbind(.,

          c("KPI_UpTime",
          as.character(seconds_to_period(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'uptime']))),

          # Server KPI's
          c("KPI_innoBufFreePerc",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'innodb_buffer_pool_pages_free']) * 100 /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'innodb_buffer_pool_pages_total'])),

          # Current innodb_buffer_pool_size
          c("KPI_bufPoolSize",
            .$VARIABLE_VALUE[.$VARIABLE_NAME == 'innodb_buffer_pool_size']),

          ## -- Total Memory Usage -- ##

          # innoBufHitrate
          c("KPI_innoBufHitrate",
            (1 - as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'innodb_buffer_pool_reads']) /
               as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'innodb_buffer_pool_read_requests'])) * 100),

          # tmpTablesOnDisk
          c("KPI_tmpTablesOnDisk",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'created_tmp_disk_tables']) /
              (as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'created_tmp_disk_tables']) +
                 as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'created_tmp_tables']))),

          # Current table_cache hit rate (OPEN_TABLES * 100/OPENED_TABLES)
          c("KPI_tblOpenCacheHitrate",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'open_tables']) * 100 /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'opened_tables'])),

          # keyReadsOnDisk
          c("KPI_keyReadsOnDisk",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'key_reads']) /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'key_read_requests'])),

          # table_cache_fill (open_tables * 100/table_open_cache)
          c("KPI_tblCacheFill",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'open_tables']) * 100 /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'table_open_cache'])),

          # Current table scan ratio (HANDLER_READ_RND_NEXT/COM_SELECT)
          c("KPI_tblScanRatio",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'handler_read_rnd_next']) /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'com_select'])),

          # query cache hitrate
          c("KPI_qryCacheHitrate",
            as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'qcache_hits']) /
              sum(as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME %in% c('qcache_hits', 'qcache_inserts', 'qcache_not_cached')]))),

          # Threads_created % of Connections"
          c("KPI_threadCacheHitrate",
            (1 - as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'threads_created']) /
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'connections']))),

          # Binlog Cache no disk use %
          c("KPI_binlogCacheNoDiskUse",
            (1 - as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'binlog_cache_disk_use']) /
               as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == 'binlog_cache_use']))),

          # Memory Allocated
          c("globalMemAllocated",
            sum(as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME %in% globalMemVars]))),

          c("perThreadMemAllocated",
            sum(as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME %in% perConnectionMemVars]))),

          c("ThreadMemAllocated",
            sum(as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME %in% perConnectionMemVars])) *
              as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME == "max_connections"])))

  innoDBstat <- innoDBstat %>%
    rbind(.,

          c("memOverallovated",
            sum(as.numeric(.$VARIABLE_VALUE[.$VARIABLE_NAME %in% c('globalMemAllocated', 'ThreadMemAllocated')])) -
              configVal("TotServerRamKb")))

  ## --------------------------------------------------------------------------------------------------##
  ## Human Readable Transformation

  # Bytes
  # !innoDBstat$VARIABLE_NAME[grep("tokudb|Tokudb", innoDBstat$VARIABLE_NAME)] &
  .tmpFormatByteVars <- "thread_stack|memOverallovated|ThreadMemAllocated|perThreadMemAllocated|globalMemAllocated" %p0%
                         "|query_cache_limit|qcache_free_memory|memory_used|size|innodb_buffer_pool_bytes_data|binlog_bytes_written"

  .editFormat <- innoDBstat %>%
    filter(!is.na(VARIABLE_VALUE) & !grepl("-", VARIABLE_VALUE)
           & grepl(.tmpFormatByteVars, VARIABLE_NAME) & !grepl("toku", VARIABLE_NAME)) %>%
    mutate(VARIABLE_VALUE = formatIECBytes(as.numeric(VARIABLE_VALUE)))

  innoDBstat <- innoDBstat %>%
    filter(!VARIABLE_NAME %in% .editFormat$VARIABLE_NAME) %>%
    union(.editFormat)

  # Percent
  innoDBstat <- innoDBstat %>%
        mutate(VARIABLE_VALUE = ifelse(VARIABLE_NAME %in% c("KPI_tmpTablesOnDisk", "KPI_keyReadsOnDisk", "KPI_qryCacheHitrate",
                                                            "KPI_threadCacheHitrate", "KPI_binlogCacheNoDiskUse"),
                                       round(100 * asNum(VARIABLE_VALUE), 2) %p0% "%",
                                       VARIABLE_VALUE
                                       )
               )

  return(innoDBstat)

}

#' Processlist
#'
#' Function to query the processlist of mariadb.
#'
#' @export
qryProcData <- function() {

  mySQLprocessList <- queryDB("SELECT `ID`, `USER`, `HOST`, `DB`, `COMMAND`, `TIME`, `PROGRESS`, `STATE`, `MEMORY_USED`,
                              `EXAMINED_ROWS`, substr(`INFO`, 1, 120) as INFO, CURRENT_TIME() as DATETIME
                              FROM `information_schema`.`PROCESSLIST`;")

  return(mySQLprocessList)

}

#' Statement Analysis
#'
#' Function to query the most often used statements. The number of results can be configured through a
#' limit in the configuration file - cnf.file - (qryStatementAnalysisLimit).
#'
#' @export
qryStmtAnalysis <- function() {

  statementAnalysisData <- queryDB("
        SELECT
          `COUNT_STAR` as exec_count
          , `AVG_TIMER_WAIT` AS `avg_latency`
          , `DIGEST_TEXT` AS `query`
          , IF(((`SUM_NO_GOOD_INDEX_USED` > 0) OR (`SUM_NO_INDEX_USED` > 0)),'*','') AS `full_scan`
          , `SCHEMA_NAME` AS `db`
          , `FIRST_SEEN` AS `first_seen`
          ,`LAST_SEEN` AS `last_seen`
        FROM `performance_schema`.`events_statements_summary_by_digest`
        ORDER BY `COUNT_STAR` DESC
        LIMIT " %p0% configVal("qryStatementAnalysisLimit") %p0% " ;")

  statementAnalysisData <- statementAnalysisData %>%
    mutate(avg_latency = round(avg_latency / 1000000000000, 3))

  return(statementAnalysisData)

}

#' Statements with Runtime in 95th Percentile
#'
#' Function to query the slowest statements. The number of results can be configured through
#' a limit in the configuration file - cnf.file - (qryFullTableScanLimit).
#'
#' @export
qry95thPercStmt <- function() {

  .subQryTopPercentile <- "
        SELECT
          `s2`.`avg_us` AS `avg_us`
          , IFNULL((SUM(`s1`.`cnt`) / NULLIF((SELECT COUNT(0)
          FROM `performance_schema`.`events_statements_summary_by_digest`),0)),0) AS `percentile`
        FROM ((
          SELECT
            COUNT(0) AS `cnt`
            , ROUND((`AVG_TIMER_WAIT` / 1000000),0) AS `avg_us`
          FROM
            `performance_schema`.`events_statements_summary_by_digest`
          GROUP BY ROUND((`AVG_TIMER_WAIT` / 1000000),0)) `s1`
        JOIN
          (SELECT
            COUNT(0) AS `cnt`
            , ROUND((`AVG_TIMER_WAIT` / 1000000),0) AS `avg_us`
          FROM
            `performance_schema`.`events_statements_summary_by_digest`
          GROUP BY
            ROUND((`AVG_TIMER_WAIT` / 1000000),0)) `s2`
            ON ((`s1`.`avg_us` <= `s2`.`avg_us`)))
        GROUP BY
          `s2`.`avg_us`
        HAVING (`percentile` > 0.95)
        ORDER BY
            IFNULL((SUM(`s1`.`cnt`) / NULLIF((SELECT COUNT(0)
            FROM `performance_schema`.`events_statements_summary_by_digest`),0)),0)
        LIMIT 1
  "

  statement95PercentileData <- queryDB("
        SELECT
          `stmts`.`COUNT_STAR` AS `exec_count`
          ,`stmts`.`AVG_TIMER_WAIT` AS `avg_latency`
          ,`stmts`.`DIGEST_TEXT` AS `query`
          ,`stmts`.`SCHEMA_NAME` AS `db`
        FROM (`performance_schema`.`events_statements_summary_by_digest` `stmts`
        JOIN (" %p0% .subQryTopPercentile %p0% ") `top_percentile`
          ON((ROUND((`stmts`.`AVG_TIMER_WAIT` / 1000000),0) >= `top_percentile`.`avg_us`)))
        ORDER BY `stmts`.`COUNT_STAR` DESC
        LIMIT " %p0% configVal("qryFullTableScanLimit") %p0% " ;")

  statement95PercentileData <- statement95PercentileData %>%
    mutate(avg_latency = round(avg_latency / 1000000000000, 1))

  return(statement95PercentileData)

}

#' Statements with full table scan
#'
#' Function to query statements with full table scan. The number of results is limited to 50.
#'
#' @export
qryFullTblScanStmt <- function() {

  fullTableScanData     <- queryDB("
        SELECT
          `COUNT_STAR` AS exec_count,
          `SCHEMA_NAME` AS db,
          `DIGEST_TEXT` AS query,
          `SUM_TIMER_WAIT` AS total_latency,
          `SUM_NO_INDEX_USED` AS no_index_used_count,
          `SUM_NO_GOOD_INDEX_USED` AS no_good_index_used_count,
          ROUND((IFNULL((`SUM_NO_INDEX_USED` / NULLIF(`COUNT_STAR`, 0)), 0) * 100), 0) AS no_index_used_pct,
          `FIRST_SEEN` AS `first_seen`,
          `LAST_SEEN` AS last_seen
        FROM `performance_schema`.`events_statements_summary_by_digest`
        WHERE (((`SUM_NO_INDEX_USED` > 0) OR (`SUM_NO_GOOD_INDEX_USED` > 0)) AND (NOT((`DIGEST_TEXT` LIKE 'SHOW%'))))
        ORDER BY `COUNT_STAR` desc
        LIMIT 50;")

  fullTableScanData <- fullTableScanData %>%
    mutate(total_latency = round(total_latency / 1000000000000, 1))

  return(fullTableScanData)

}

#' Statements with tmp_disk_tables
#'
#' Function to query statements with temporary tables.
#'
#' @export
qryTmpDiscTblStmt <- function() {

  tmpDiscTableStatementData  <- queryDB("
      SELECT `DIGEST_TEXT` AS `query`
        , `SCHEMA_NAME` AS `db`
        , `COUNT_STAR` AS `exec_count`
        , `SUM_TIMER_WAIT` AS `total_latency`
        , MIN_TIMER_WAIT AS `min_latency`
        , AVG_TIMER_WAIT AS `avg_latency`
        , `SUM_CREATED_TMP_TABLES` AS `memory_tmp_tables`
        , `SUM_CREATED_TMP_DISK_TABLES` AS `disk_tmp_tables`
        , ROUND(IFNULL((`SUM_CREATED_TMP_TABLES` / NULLIF(`COUNT_STAR`, 0)), 0), 0) AS `avg_tmp_tables_per_query`
        , ROUND((IFNULL((`SUM_CREATED_TMP_DISK_TABLES` / NULLIF(`SUM_CREATED_TMP_TABLES`, 0)), 0) * 100), 0) AS `tmp_tables_to_disk_pct`
        , `FIRST_SEEN` AS `first_seen`
        , `LAST_SEEN` AS `last_seen`
      FROM `performance_schema`.`events_statements_summary_by_digest`
      WHERE (`SUM_CREATED_TMP_DISK_TABLES` > 0) -- and (`AVG_TIMER_WAIT`/1000000000000 > 0.1)
      ORDER BY `avg_latency` DESC;")

  tmpDiscTableStatementData <- tmpDiscTableStatementData %>%
    mutate(total_latency = round(total_latency / 1000000000000, 1),
           min_latency = round(min_latency / 1000000000000, 1),
           avg_latency = round(avg_latency / 1000000000000, 1))

  return(tmpDiscTableStatementData)

}

#' User Statistic
#'
#' Function to query statistics of the mariadb users. The number of results can be configured through
#' a limit in the configuration file - cnf.file - (qryUserStatLimit).
#'
#' @export
qryUserStat <- function() {

  .subQryStmt <- "
      SELECT
        IF(ISNULL(`USER`),'background',`USER`) AS `user`
        , SUM(`COUNT_STAR`) AS `total`
        , SUM(`SUM_TIMER_WAIT`) AS `total_latency`
        ,(SUM(`SUM_NO_INDEX_USED`) + SUM(`SUM_NO_GOOD_INDEX_USED`)) AS `full_scans`
        ,(SUM(`SUM_WARNINGS`) + SUM(`SUM_ERRORS`)) AS `warn_err`
        , SUM(`SUM_CREATED_TMP_TABLES`) as tmp_tbls
        , SUM(`SUM_CREATED_TMP_DISK_TABLES`) as tmp_disk_tbls
        , SUM(`SUM_ROWS_SENT`) as rows_send
      FROM
        `performance_schema`.`events_statements_summary_by_user_by_event_name`
      GROUP BY
         IF(ISNULL(`USER`),'background',`USER`)"

  .subQryIo <- "
      SELECT
        IF(ISNULL(`USER`)
        ,'background',`USER`) AS `user`
        , SUM(`COUNT_STAR`) AS `ios`
        , SUM(`SUM_TIMER_WAIT`) AS `io_latency`
      FROM
        `performance_schema`.`events_waits_summary_by_user_by_event_name`
      GROUP BY
        IF(ISNULL(`USER`),'background',`USER`)"

  userStatData <- queryDB("
      SELECT
        IF(ISNULL(`accounts`.`USER`),'background',`accounts`.`USER`) AS `user`
        , SUM(`stmt`.`total`) AS `statements`, SUM(`stmt`.`total_latency`) AS `stmt_latency`
        , IFNULL((SUM(`stmt`.`total_latency`) / NULLIF(SUM(`stmt`.`total`),0)),0) AS `avg_latency`
        , SUM(`stmt`.`full_scans`) AS `tbl_scans`
        , SUM(`io`.`io_latency`) AS `io_latency`
        , SUM(`stmt`.`warn_err`) AS `warn_err`
        , SUM(`stmt`.`tmp_tbls`) as tmp_tbls
        , SUM(`stmt`.`tmp_disk_tbls`) as tmp_disk_tbls
        , SUM(`stmt`.`rows_send`) as rows_send
        , SUM(`accounts`.`CURRENT_CONNECTIONS`) AS `current_con`
        , SUM(`accounts`.`TOTAL_CONNECTIONS`) AS `tot_con`
        , COUNT(DISTINCT `accounts`.`HOST`) AS `unique_hosts`
     FROM
        ((`performance_schema`.`accounts`
     LEFT JOIN (" %p0% .subQryStmt %p0% ") `stmt`
        ON ((IF(ISNULL(`accounts`.`USER`),'background',`accounts`.`USER`) = `stmt`.`user`)))
     LEFT JOIN (" %p0% .subQryIo %p0% ") `io`
        ON ((IF(ISNULL(`accounts`.`USER`),'background',`accounts`.`USER`) = `io`.`user`)))
     GROUP BY
        IF(ISNULL(`accounts`.`USER`),'background',`accounts`.`USER`)
     ORDER BY
        SUM(`accounts`.`TOTAL_CONNECTIONS`) desc
     LIMIT " %p0% configVal("qryUserStatLimit") %p0% " ;") %>%
    mutate(
      stmt_latency = as.character(seconds_to_period(
        round(as.numeric(stmt_latency) / 1000000000000, 0))),
      avg_latency = round(avg_latency / 1000000000000, 1),
      io_latency = round(io_latency / 1000000000000, 0)
      )

  return(userStatData)

}

#' event Data
#'
#' Function to query statistics for the events of the event scheduler.
#'
#' @export
qryEventData <- function() {

  eventData <- queryDB("SELECT db, name, `definer`, created, modified, last_executed, `status`, `comment`
                       FROM `mysql`.`event`;")

  return(eventData)

}

#' unsused Indexes
#'
#' Function to query statistics for unused table indices since last restart.
#'
#' @export
qryIdxData <- function() {

  indexData <- queryDB("
        SELECT
          `OBJECT_SCHEMA` AS `object_schema`,
          t1.`OBJECT_NAME` AS `object_name`,
          t1.`INDEX_NAME` AS `index_name`,
          max(`CARDINALITY`) as `CARDINALITY`
        FROM
          `performance_schema`.`table_io_waits_summary_by_index_usage` t1
        join
          information_schema.STATISTICS t2 on t1.OBJECT_SCHEMA = t2.TABLE_SCHEMA
         and
          t1.OBJECT_NAME = t2.TABLE_NAME and t1.INDEX_NAME = t2.INDEX_NAME
        WHERE
          ((t1.`INDEX_NAME` IS NOT NULL) AND (`COUNT_STAR` = 0)
         AND
          (`OBJECT_SCHEMA` <> 'mysql')
         AND
          (t1.`INDEX_NAME` <> 'PRIMARY'))
        group by
          `OBJECT_SCHEMA`
          , t1.`OBJECT_NAME`
          , t1.`INDEX_NAME`
        ORDER BY
          t1.`OBJECT_SCHEMA`
          , t1.`INDEX_NAME`
          , max(`CARDINALITY`);")

  return(indexData)

}

#' Log writes
#'
#' Function to query statistics of log writes of mariadb.
#'
#' @export
qryLogWrites <- function() {

  filter <- ifelse(qryFlagTokuEngine(),
                   "'Tokudb_logger_writes_bytes', 'Tokudb_logger_writes', 'Innodb_os_log_written',
                     'Innodb_log_write_requests', 'Innodb_log_writes'",
                   "'Innodb_os_log_written', 'Innodb_log_write_requests', 'Innodb_log_writes'")

  logWriteData <- queryDB("Select * , CURRENT_TIMESTAMP() as DATETIME, NULL as VARIABLE_VALUE_SEC from information_schema.GLOBAL_STATUS
                          where VARIABLE_NAME in (" %p0% filter %p0% ");")

  logWriteData <- logWriteData %>%
    mutate(VARIABLE_VALUE = ifelse(VARIABLE_NAME %in% c("INNODB_OS_LOG_WRITTEN", "TOKUDB_LOGGER_WRITES_BYTES"),
                                 as.numeric(VARIABLE_VALUE) / 1024 / 1024,
                                 VARIABLE_VALUE),
           VARIABLE_NAME = ifelse(VARIABLE_NAME %in%  c("INNODB_OS_LOG_WRITTEN", "TOKUDB_LOGGER_WRITES_BYTES"), "LOG_WRITES_OS_MB",
                                VARIABLE_NAME),
           VARIABLE_NAME = ifelse(VARIABLE_NAME %in%  c("INNODB_LOG_WRITES", "TOKUDB_LOGGER_WRITES"), "LOG_WRITES", VARIABLE_NAME)) %>%
    group_by(VARIABLE_NAME) %>%
    summarise(VARIABLE_VALUE = sum(as.numeric(VARIABLE_VALUE)),
              DATETIME = first(DATETIME),
              VARIABLE_VALUE_SEC = NA) %>%
    data.frame

  return(logWriteData)

}

#' InnoDB buffer reads
#'
#' Function to query statistics of buffer reads of mariadb.
#'
#' @export
qryBufferReads <- function() {

  bufferReadsData <- queryDB("Select * , CURRENT_TIMESTAMP() as DATETIME, NULL as VARIABLE_VALUE_SEC from information_schema.GLOBAL_STATUS
                             where VARIABLE_NAME in ('innodb_buffer_pool_reads', 'innodb_buffer_pool_read_requests',
                             'Innodb_buffer_pool_write_requests');")

  return(bufferReadsData)

}

#' InnoDB Status
#'
#' Function to query operational information about the innodb storage engine.
#'
#' @export
qryInnoDBStatus <- function() {

  innoDBStatus <- queryDB("SHOW ENGINE INNODB STATUS;")

  return(innoDBStatus)

}

#' Index Cardinality
#'
#' Function to query the cardinality of table indices.
#'
#' @export
qryIdxCardinality <- function() {

  indexCardinality <- queryDB("SELECT `CARDINALITY`, TABLE_SCHEMA, TABLE_NAME, INDEX_NAME, GROUP_CONCAT(`COLUMN_NAME`) as `COLUMN_NAME`,
                              NON_UNIQUE
                              FROM information_schema.STATISTICS
                              where TABLE_SCHEMA not in ('mysql', 'sys') and INDEX_NAME <> 'PRIMARY'
                              group by TABLE_SCHEMA, TABLE_NAME, INDEX_NAME, `CARDINALITY`, NON_UNIQUE
                              order by CARDINALITY;")

  return(indexCardinality)

}

#' Index with null values
#'
#' Function to query the indices with null values.
#'
#' @export
qryIdxNullable <- function() {

  indexNullable <- queryDB("
        SELECT
          `OBJECT_SCHEMA` AS `object_schema`,
           t1.`OBJECT_NAME` AS `object_name`,
           t1.`INDEX_NAME` AS `index_name`,
           `NULLABLE` as `nullable`,
           max(`CARDINALITY`) as `CARDINALITY`
        FROM `performance_schema`.`table_io_waits_summary_by_index_usage` t1
        join information_schema.STATISTICS t2
          on t1.OBJECT_SCHEMA = t2.TABLE_SCHEMA
          and t1.OBJECT_NAME = t2.TABLE_NAME and t1.INDEX_NAME = t2.INDEX_NAME
        WHERE ((t1.`INDEX_NAME` IS NOT NULL) AND (`NULLABLE` = 'YES') AND (`OBJECT_SCHEMA` <> 'mysql') AND (t1.`INDEX_NAME` <> 'PRIMARY'))
        GROUP BY `OBJECT_SCHEMA`, t1.`OBJECT_NAME`, t1.`INDEX_NAME`
        ORDER BY t1.`OBJECT_SCHEMA`, t1.`INDEX_NAME`, `NULLABLE`, max(`CARDINALITY`);")

  return(indexNullable)

}

#' Statements with warnings or errors.
#'
#' Function to query statements with errors or warnings. The number of results can be configured through a limit in the
#' configuration file - cnf.file - (qryFilterErrors, qryFilterWarnings). Statements with more errors or warnings then the threshold
#' will be in the result set.
#'
#' @export
qryErrWarnData <- function() {

  errWarnData <- queryDB("
        SELECT
          `DIGEST_TEXT` AS `query`,
          `SCHEMA_NAME` AS `db`,
          `COUNT_STAR` AS `exec_count`,
          `SUM_ERRORS` AS `errors`,
          (IFNULL((`SUM_ERRORS` / NULLIF(`COUNT_STAR`, 0)), 0) * 100) AS `error_pct`,
          `SUM_WARNINGS` AS `warnings`,
          (IFNULL((`SUM_WARNINGS` / NULLIF(`COUNT_STAR`, 0)), 0) * 100) AS `warning_pct`,
          `FIRST_SEEN` AS `first_seen`,
          `LAST_SEEN` AS `last_seen`
        FROM `performance_schema`.`events_statements_summary_by_digest`
        WHERE
              `SUM_ERRORS` >" %p% configVal("qryFilterErrors") %p%
            "or
              `SUM_WARNINGS` >" %p% configVal("qryFilterWarnings") %p% "
        order by `COUNT_STAR` desc;")

  return(errWarnData)

}

#' Check if TokuDB-Storage-Engine is in use
#'
#' Function to check if TokuDB-Storage-Engine is in use
#'
#' @export
qryFlagTokuEngine <- function() {

  flagTokuEngine <- queryDB("SELECT if(sum(if(`ENGINE` = 'TokuDB',1,0)) = 0, 0, 1) as flagTokuEngine
                             FROM information_schema.`TABLES`;")

  return(flagTokuEngine$flagTokuEngine)

}

#' Query maxscale user
#'
#' Function to query maxinfo
#'
#' @export
qryMaxInfo <- function(what) {

  if (is.null(procMaxscale(mySQLprocessList()))) return(NULL)

  queryDB("show " %p0% what, "host=" %p0% procMaxscale(mySQLprocessList())$HOST, "port=" %p0% procMaxscale(mySQLprocessList())$PORT)

}

#' Query host cache
#'
#' Function to query host cache
#'
#' @export
qryHostCache <- function() {

  qryHostCacheData <- queryDB("
    Select
      IP as ip,
      HOST as host,
      SUM_CONNECT_ERRORS AS con_err,
      COUNT_HANDSHAKE_ERRORS AS handshake_err,
      COUNT_AUTHENTICATION_ERRORS AS auth_err,
      COUNT_SSL_ERRORS AS ssl_err,
      COUNT_MAX_USER_CONNECTIONS_ERRORS AS max_user_con_err,
      COUNT_FCRDNS_ERRORS AS fcrdns_err,
      FIRST_SEEN AS first_seen,
      LAST_SEEN AS last_seen,
      FIRST_ERROR_SEEN AS first_err_seen,
      LAST_ERROR_SEEN AS last_err_seen
    FROM performance_schema.host_cache
  where FIRST_ERROR_SEEN is not NULL;")

  return(qryHostCacheData)

}
