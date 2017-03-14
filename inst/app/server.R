library("INWTdbMonitor")

# load init config
source(paste0(system.file("app", package = "INWTdbMonitor"), "/cnf.R"))
dbConfig$init()

shinyServer(function(input, output, session) {

  # define schedule for reloading data (reactive function) ---------------------------------
  statement95PercentileData <- reactive({
    input$serverList
    reloadBySchedule(dat = qry95thPercStmt(), schedule = "daily")
    })

  indexCardinality <- reactive({
    input$serverList
    reloadBySchedule(dat = qryIdxCardinality(), schedule = "daily")
    })

  indexNullable <- reactive({
    input$serverList
    reloadBySchedule(dat = qryIdxNullable(), schedule = "daily")
  })

  fullTableScanData <- reactive({
    input$serverList
    reloadBySchedule(dat = qryFullTblScanStmt(), schedule = "daily")
    })

  statementAnalysis <- reactive({
    input$serverList
    reloadBySchedule(dat = qryStmtAnalysis(), schedule = "daily")
    })

  tmpDiscTableStatementData <- reactive({
    input$serverList
    reloadBySchedule(dat = qryTmpDiscTblStmt(), schedule = "daily")
    })

  userStat <- reactive({
    input$serverList
    reloadBySchedule(dat = qryUserStat(), schedule = "daily")
    })

  innoDBStatus <- reactive({
    input$serverList
    reloadBySchedule(dat = qryInnoDBStatus(), schedule = "daily")
    })

  eventData <- reactive({
    input$serverList
    reloadBySchedule(dat = qryEventData(), schedule = "daily")
    })

  indexData <- reactive({
    input$serverList
    reloadBySchedule(dat = qryIdxData(), schedule = "daily")
    })

  errWarnData <- reactive({
    input$serverList
    reloadBySchedule(dat = qryErrWarnData(), schedule = "daily")
    })

  innoDBstat <- reactive({
    input$serverList
    reloadBySchedule(dat = qryServStatData(), schedule = "each5Seconds")
    })

  mySQLprocessList <- reactive({
    input$serverList
    reloadBySchedule(dat = qryProcData(), schedule = "each5Seconds")
    })

  hostCache <- reactive({
    input$serverList
    reloadBySchedule(dat = qryHostCache(), schedule = "daily")
  })

  # Select DB-Servers in Master-Slave-Replication ---------------------------------
  output$serverList <- renderUI({
    selectInput(inputId = "serverList", label = h4("Select server:"),
                choices =  Filter(Negate(is.null), list(initDbServer(), procSlaveServer(mySQLprocessList())$HOST))
    )
  })

  # if selection chages, update db-credentials
  observeEvent(input$serverList,
               {dbConfig$set(input$serverList)
  }
  )

  # create static values ---------------------------------
  output$buffTot <- renderText(ifelse(qryFlagTokuEngine(),
       paste0("Buffer Pool (", paste0(formatIECBytes(serverValNum(cleanVarList(innoDBstat()),
            c("KPI_bufPoolSize", "tokudb_cache_size"))), collapse = " + "), ")"),
       paste0("InnoDB Buffer Pool (", formatIECBytes(serverValNum(cleanVarList(innoDBstat()), "KPI_bufPoolSize")), ")")
  ))

  output$maxCon     <- renderText(paste0("Maximum number of connections: ", serverVal(cleanVarList(innoDBstat()),
                          "max_connections"), " (multiplied with following values leading to maximum use of ",
                          serverVal(cleanVarList(innoDBstat()), "ThreadMemAllocated"), " total thread memory and ",
                          serverVal(cleanVarList(innoDBstat()), "memOverallovated"), " overallocated memory)"))
  # html manipulations ---------------------------------
  output$innoDBStatus <- renderUI({

                obj <-  innoDBStatus()[, 3]
                HTML(
                  "<p style = 'margin-left: 20px;'>",
                  input$serverList,
                  ":<br/>", gsub("\\n", "<br/>", obj),
                  "</p>"
                )
  })

  # create reactive values ---------------------------------
  values <- reactiveValues()

  values$numUsers    <- data.frame(TOT_CONNECTIONS = NA, TOT_MEMORY = NA, RUN_CONNECTIONS = NA)
  values$logWrite    <- data.frame(VARIABLE_NAME = NA, VARIABLE_VALUE = NA, DATETIME = NA, VARIABLE_VALUE_SEC = NA)
  values$bufferReads <- data.frame(VARIABLE_NAME = NA, VARIABLE_VALUE = NA, DATETIME = NA, VARIABLE_VALUE_SEC = NA)

  newEntry <- observe({
    numUserDat <- procToTimeLine(mySQLprocessList())
    isolate(values$numUsers <- rbind(values$numUsers, numUserDat))

    logWriteDat <- cleanLogWrites(innoDBstat())
    isolate(values$logWrite <- helperBufferWrite(values$logWrite, logWriteDat))

    bufferReadsDat <- bufferReads(innoDBstat())
    isolate(values$bufferReads <- helperBufferWrite(values$bufferReads, bufferReadsDat))

  })

  # render tables ---------------------------------

  # simple renderTable
  output$tblEventStat <- renderTable({
    eventData()
  })

  output$tblMemStatGlob <- renderTable({
    dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% globalMemVars)
  })

  output$tblMemStatThread <- renderTable({
    dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% perConnectionMemVars)
  })

  output$tblVariables <- renderDataTable({
    cleanVarList(innoDBstat())
  })

  output$tblProcessActive <- renderTable({
    dplyr::filter(cleanProcList(mySQLprocessList()),  COMMAND != "Sleep")
  })

  output$tblProcessSleep <- renderTable({
    dplyr::filter(cleanProcList(mySQLprocessList()),  COMMAND == "Sleep")
  })

  # DT:renderDataTables
  output$tblTmpTables <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tmpTableStatVars)))
    )
  })

  output$tblTblLock <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblLockingVars)))
    )
  })

  output$tblSlowQry <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% slowQryVars)))
    )
  })

  output$tblWorkThread <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% workerThreadsVars)))
    )
  })

  output$tblKeyBuffSize <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% keyBufferSizeVars)))
    )
  })

  output$tblQryCache <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% qryCacheVars)))
    )
  })

  output$tblSortOp <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% sortOpVars)))
    )
  })

  output$tblJoins <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% joinVars)))
    )
  })

  output$tblTblScan <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblScansVars)))
    )
  })

  output$tblBinlogCache <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblBinlogVars)))
    )
  })

  output$tblAbortCon <- DT::renderDataTable({
    appDataTable(isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% abortedCons)))
    )
  })

  output$tblUsedCon <- DT::renderDataTable({
    datatable(isolate(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% usedConVars)),
              options = list(pageLength = 50, searching = FALSE, paging = FALSE, info = FALSE)) %>%
      formatCurrency(columns = 'VARIABLE_VALUE', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'VARIABLE_VALUE',
        background = styleColorBar(as.numeric(isolate(dplyr::filter(cleanVarList(innoDBstat()),
                      VARIABLE_NAME %in% usedConVars)$VARIABLE_VALUE)), 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # proxy for data tables
  proxyTblTmpTables <- dataTableProxy('tblTmpTables')
  proxyTblTblLock <- dataTableProxy('tblTblLock')
  proxyTblSlowQry <- dataTableProxy('tblSlowQry')
  proxyTblWorkThread <- dataTableProxy('tblWorkThread')
  proxyTblKeyBuffSize <- dataTableProxy('tblKeyBuffSize')
  proxyTblQryCache <- dataTableProxy('tblQryCache')
  proxyTblSortOp <- dataTableProxy('tblSortOp')
  proxyTblJoins <- dataTableProxy('tblJoins')
  proxyTblTblScan <- dataTableProxy('tblTblScan')
  proxyTblBinlogCache <- dataTableProxy('tblBinlogCache')
  proxyTblAbortCon <- dataTableProxy('tblAbortCon')
  proxyTblUsedConn <- dataTableProxy('tblUsedCon')

  # observe for data tables
  observe({
    replaceData(proxyTblTmpTables,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tmpTableStatVars))), resetPaging = FALSE)
    replaceData(proxyTblTblLock,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblLockingVars))), resetPaging = FALSE)
    replaceData(proxyTblSlowQry,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% slowQryVars))), resetPaging = FALSE)
    replaceData(proxyTblWorkThread,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% workerThreadsVars))), resetPaging = FALSE)
    replaceData(proxyTblKeyBuffSize,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% keyBufferSizeVars))), resetPaging = FALSE)
    replaceData(proxyTblQryCache,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% qryCacheVars))), resetPaging = FALSE)
    replaceData(proxyTblSortOp,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% sortOpVars))), resetPaging = FALSE)
    replaceData(proxyTblJoins,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% joinVars))), resetPaging = FALSE)
    replaceData(proxyTblTblScan,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblScansVars))), resetPaging = FALSE)
    replaceData(proxyTblBinlogCache,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% tblBinlogVars))), resetPaging = FALSE)
    replaceData(proxyTblAbortCon,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% abortedCons))), resetPaging = FALSE)
    replaceData(proxyTblUsedConn,
      isolate(transformThousends(dplyr::filter(cleanVarList(innoDBstat()), VARIABLE_NAME %in% usedConVars))), resetPaging = FALSE)
  })

  # DT:renderDataTables (no replaceData)
  output$tblExecFreq <- DT::renderDataTable({
    datatable(statementAnalysis(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'exec_count', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'avg_latency',
        color = styleInterval(c(configVal("threshSlowQryOrange"), configVal("threshSlowQryRed")), c('grey', 'orange', 'red'))
      ) %>%
        formatStyle(
          'exec_count',
          background = styleColorBar(statementAnalysis()$exec_count, 'steelblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
  }
  )

  output$tblErrWarnData <- DT::renderDataTable({
    datatable(errWarnData(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = c('exec_count', 'errors', 'warnings'), currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        c('error_pct', 'warning_pct'),
        color = styleInterval(c(configVal("threshWarnQryPercOrange"), configVal("threshErrQryPercRed")), c('grey', 'orange', 'red'))
      ) %>%
      formatStyle(
        'exec_count',
        background = styleColorBar(errWarnData()$exec_count, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        c('errors', 'warnings'),
        background = styleColorBar(errWarnData()$errors, 'steelblue'),
        backgroundSize = '90% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatRound(c('error_pct', 'warning_pct'), 0)
  }
  )

  output$tblRun95thPerc <- DT::renderDataTable({
    datatable(statement95PercentileData(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'exec_count', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'avg_latency',
        color = styleInterval(c(configVal("threshSlowQryOrange"), configVal("threshSlowQryRed")), c('grey', 'orange', 'red'))
      ) %>%
      formatStyle(
        'exec_count',
        background = styleColorBar(statement95PercentileData()$exec_count, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tblTmpDiscTableStmt <- DT::renderDataTable({
    datatable(tmpDiscTableStatementData(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'exec_count', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'avg_latency',
        color = styleInterval(c(configVal("threshSlowQryOrange"), configVal("threshSlowQryRed")), c('grey', 'orange', 'red'))
      ) %>%
      formatStyle(
        'exec_count',
        background = styleColorBar(tmpDiscTableStatementData()$exec_count, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'disk_tmp_tables',
        background = styleColorBar(tmpDiscTableStatementData()$disk_tmp_tables, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tblindexCardinality <- DT::renderDataTable({
    datatable(indexCardinality(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'CARDINALITY', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'CARDINALITY',
        background = styleColorBar(indexCardinality()$CARDINALITY, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tblindexNullable <- DT::renderDataTable({
    datatable(indexNullable(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'CARDINALITY', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'CARDINALITY',
        background = styleColorBar(indexCardinality()$CARDINALITY, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tableUserStat <- DT::renderDataTable({
    datatable(userStat(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = c('statements', 'tbl_scans', 'current_con', 'tot_con', 'unique_hosts',
                                 'warn_err', 'tmp_tbls', 'tmp_disk_tbls', 'rows_send'),
                     currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'statements',
        background = styleColorBar(userStat()$statements, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tblUsedIndex <- DT::renderDataTable({
    datatable(indexData(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = 'CARDINALITY', currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'CARDINALITY',
        background = styleColorBar(indexData()$CARDINALITY, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  )

  output$tblFullTableScan <- DT::renderDataTable({
    datatable(fullTableScanData(), options = list(pageLength = 50)) %>%
      formatCurrency(columns = c('exec_count', 'no_index_used_count', 'no_good_index_used_count'),
                     currency = "", interval = 3, mark = " ", digits = 0) %>%
      formatStyle(
        'exec_count',
        background = styleColorBar(fullTableScanData()$exec_count, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'no_index_used_pct',
        color = styleInterval(c(50, 99), c('black', 'orange', 'red'))
      )
  }
  )

  output$tblHostCache <- DT::renderDataTable({
    datatable(hostCache(), options = list(pageLength = 50)) %>%
      formatStyle(
        c('con_err', 'handshake_err', 'auth_err', 'ssl_err',
          'max_user_con_err', 'fcrdns_err'),
        color = styleInterval(c(0, 1), c('black', 'orange', 'red'))
      )
  }
  )


  MaxInfoWhat <- c("status", "servers", "variables", "sessions", "clients", "services", "listeners", "modules", "eventTimes")
  for (what in MaxInfoWhat) {

    outName <- what

    local({

      whatLocal <- what

      output[[outName]] <- DT::renderDataTable({
        datatable(qryMaxInfo(mySQLprocessList(), whatLocal), options = list(pageLength = 50))
      })

    })

  }

  # server side rendering of maxinfo tables
  output$maxScaleTabs <- renderUI({
    myTabs <- lapply(MaxInfoWhat, function(x) tabPanel(title = x, DT::dataTableOutput(x)))
    do.call(tabBox, myTabs)
  })

  # render editable Tables ---------------------------------
  values <- reactiveValues(appVariables = data.frame(appConfig))

  observe({
    if (!is.null(input$appVariables))
      values$appVariables <- hot_to_r(input$appVariables)
      write.csv2(values$appVariables, "app.cnf", row.names = FALSE, quote = FALSE)
  })

  output$appVariables <- renderRHandsontable({
    rhandsontable(values$appVariables) %>%
      hot_col(col = "value", type = "numeric")
  })

  # visualisations ---------------------------------
  # Visualisation of Process over time
  output$dygraphProcess <- renderDygraph({
    dygraph(values$numUsers) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Number", valueRange = c(0, serverVal(cleanVarList(innoDBstat()), "max_connections"))) %>%
      dyAxis("y2", label = "Megabyte", independentTicks = TRUE) %>%
      dySeries("TOT_CONNECTIONS", fillGraph = TRUE, color = "#3F51B5") %>%
      dySeries("RUN_CONNECTIONS", fillGraph = TRUE, color = "#2196F3") %>%
      dySeries("TOT_MEMORY", axis = 'y2', color = "#607D8B", strokeWidth = 3, strokePattern = "dashed") %>%
      dyOptions(includeZero = TRUE,
                axisLineColor = "navy",
                gridLineColor = "lightblue")  %>%
      dyLegend(width = 1000) %>%
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  })

  # Visualisation of IO over time
  output$dygraphLogWrite <- renderDygraph({
    dygraph(helperDygraphDat(values$logWrite)) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Num/sec") %>%
      dyAxis("y2", label = "MB/sec", independentTicks = TRUE) %>%
      dySeries("INNODB_LOG_WRITE_REQUESTS", fillGraph = TRUE, color = "#3F51B5") %>%
      dySeries("LOG_WRITES", fillGraph = TRUE, color = "#2196F3") %>%
      dySeries("LOG_WRITES_OS_MB", axis = 'y2', color = "#607D8B", strokeWidth = 3, strokePattern = "dashed") %>%
      dyOptions(includeZero = TRUE,
                axisLineColor = "navy",
                gridLineColor = "lightblue") %>%
      dyLegend(width = 1000) %>%
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  })

  # Visualisation of InnoDB-Buffer-Reads over time
  output$dygraphBufferReads <- renderDygraph({
    dygraph(helperDygraphDat(values$bufferReads)) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Num/sec") %>%
      dyAxis("y2", label = "Num/sec", independentTicks = TRUE) %>%
      dySeries("INNODB_BUFFER_POOL_READS", fillGraph = TRUE, color = "#3F51B5") %>%
      dySeries("INNODB_BUFFER_POOL_READ_REQUESTS", fillGraph = TRUE, color = "#2196F3") %>%
      dySeries("INNODB_BUFFER_POOL_WRITE_REQUESTS", axis = 'y2', color = "#607D8B", strokeWidth = 3, strokePattern = "dashed") %>%
      dyOptions(includeZero = TRUE,
                axisLineColor = "navy",
                gridLineColor = "lightblue") %>%
      dyLegend(width = 1400) %>%
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  })

  # PieCharts
  output$plotBufferFree <- ifelse(qryFlagTokuEngine(),
                                  renderGvis({
                                    gvisBarChart(isolate(bufferDat(cleanVarList(innoDBstat()))), xvar = "engine",
                                                 yvar = c("Filled", "Free"),
                                                 options = list(hAxis = "{format:'#,###%'}", isStacked = TRUE,
                                                                colors = "['#4CAF50','#FF9800']"))
                                  })
                                  ,
                                  renderGvis({
                                    appPieChart(label = c("Free", "Filled"),
                                                value = isolate(
                                                  c(serverValNum(cleanVarList(innoDBstat())), "innodb_buffer_pool_pages_free",
                                                    serverValNum(cleanVarList(innoDBstat())), "innodb_buffer_pool_pages_total" -
                                                    serverValNum(cleanVarList(innoDBstat()))), "innodb_buffer_pool_pages_free")
                                    )
                                  })
  )

  output$plotBufferHitrate <- renderGvis({
    appPieChart(label = c("Reads", "Requests"),
    value = isolate(c(serverValNum(cleanVarList(innoDBstat()), "innodb_buffer_pool_reads"),
                      serverValNum(cleanVarList(innoDBstat()), "innodb_buffer_pool_read_requests")))
    )
  })

  output$plotTableCacheHitrate <- renderGvis({
    appPieChart(label = c("Closed", "Open"),
    value = isolate(c(serverValNum(cleanVarList(innoDBstat()), "opened_tables") -
                serverValNum(cleanVarList(innoDBstat()), "open_tables"), serverValNum(cleanVarList(innoDBstat()), "open_tables")))
    )
  })

  output$plotTempTables <- renderGvis({
    appPieChart(label = c("On disk", "In memory"),
    value = isolate(c(serverValNum(cleanVarList(innoDBstat()), "created_tmp_disk_tables"),
              serverValNum(cleanVarList(innoDBstat()), "created_tmp_tables"))), badColor = "#F44336"
    )
  })


  # create dynmaic notification messages ---------------------------------
  # for sidebar
  output$menuNote <- renderUI({
    messageData <- data.frame(text = c(paste0("Up-Time: ", serverVal(cleanVarList(innoDBstat()), "KPI_UpTime")),
                                       paste0("No. Users: ", length(unique(mySQLprocessList()$USER))),
                                       paste0("No. Processes: ", length(mySQLprocessList()$USER)),
                                       paste0("Open Tables: ", serverVal(cleanVarList(innoDBstat()), "open_tables")),
                                       paste0("Buffer: ",
                                              ifelse(qryFlagTokuEngine(),
                                                     formatIECBytes(bufferTotDat(cleanVarList(innoDBstat()))),
                                                     serverVal(cleanVarList(innoDBstat()), "innodb_buffer_pool_bytes_data")
                                                     )
                                              ),
                                       paste0("Mem Used: ", serverVal(cleanVarList(innoDBstat()), "memory_used"))
                           ),
               icon = c("clock-o", "user", "terminal", "table", "pie-chart", "pie-chart"),
               link = c("#shiny-tab-dashboard", "#shiny-tab-userStatTab", "#shiny-tab-dashboard",
                        "#shiny-tab-dashboard", "#shiny-tab-memStat", "#shiny-tab-memStat")
    )
    msgs <- apply(messageData, 1, function(row) {
      notificationItem(text = tags$div(row[["text"]], style = "display: inline-block; vertical-align: middle;padding: 0px 0px 0px 30px;"),
                       icon = icon(row[["icon"]]))
    })
    lapply(1:length(msgs), function(x) linkToTab(messageData$link[x], msgs[[x]]))
  })

  # for top message bar
  output$dropdownMenuNote <- renderUI({

    tmpTotServerRamGB <- configVal("TotServerRamKb") / 1024 / 1024 / 1024
    messageData <- data.frame(text = c(paste0("Mem allocated global: ", serverVal(cleanVarList(innoDBstat()), "globalMemAllocated")),
                                       paste0("Mem allocated per thread: ",
                                              serverVal(cleanVarList(innoDBstat()), "perThreadMemAllocated")),
                                       paste0("Mem allocated per thread (max): ",
                                              serverVal(cleanVarList(innoDBstat()), "ThreadMemAllocated")),
                                       paste0("Overallocated memory: ", serverVal(cleanVarList(innoDBstat()), "memOverallovated")),
                                       paste0("", nrow(indexData()), " unused indexes since last restart"),
                                       paste0(nrow(statementAnalysis()[as.numeric(gsub(" sec", "",
                                                                                       statementAnalysis()$avg_latency)) > 1, ]),
                                              " top 50 query with over 1 sec runtime"),
                                       paste0(nrow(statement95PercentileData()[as.numeric(
                                         statement95PercentileData()$exec_count) > 3, ]),
                                              " slow queries exec. more then three times")
    ),
    value   = c(serverVal(cleanVarList(innoDBstat()), "globalMemAllocated"),
                serverVal(cleanVarList(innoDBstat()), "perThreadMemAllocated"),
                serverVal(cleanVarList(innoDBstat()), "ThreadMemAllocated"),
                serverVal(cleanVarList(innoDBstat()), "memOverallovated"),
                nrow(indexData()),
                nrow(statementAnalysis()[as.numeric(gsub(" sec", "", statementAnalysis()$avg_latency)) > 1, ]),
                nrow(statement95PercentileData()[as.numeric(statement95PercentileData()$exec_count) > 3, ])),
    icon = c("pie-chart", "pie-chart", "pie-chart", "stack-overflow", "book", "hourglass", "spinner"),
    link = c("", "", "", "#shiny-tab-memStat", "#shiny-tab-indexData", "#shiny-tab-statementAnalysisTab",
             "#shiny-tab-statementAnalysisTab"),
    threshold = c(tmpTotServerRamGB, tmpTotServerRamGB, tmpTotServerRamGB, 0, 10, 0, 0)
    )
    messageData$status <- ifelse(as.numeric(gsub("[a-zA-Z]", "", messageData$value)) > messageData$threshold, "danger", "info")
    messageData <- messageData[c(5, 6, 7, 4, 1, 2, 3), ]

    msgs <- apply(messageData, 1, function(row) {
      notificationItem(text = row[["text"]], status = row[["status"]], icon = icon(row[["icon"]]))
    })
    msgs <- lapply(1:length(msgs), function(x) linkToTab(messageData$link[x], msgs[[x]]))

    dropdownMenu(type = "notifications", .list = msgs, badgeStatus = ifelse(any(messageData$status == "danger"), "danger", "primary"))
  })

  # observe events to link to specific a tabsetpanel ---------------------------------
  observeEvent(input$panelTmpTbl, {
    newvalue <- "Statements with temp disc tables"
    updateTabsetPanel(session, "panelStatements", newvalue)
  })

})

