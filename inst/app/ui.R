###############################################################################

library("INWTdbMonitor")

# header elements for the visualization ---------------------------------
header <- dashboardHeader(title = "MariaDB Monitor",
                          dropdownMenuOutput("dropdownMenuNote"),
                          dropdownMenu(
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success"
                          )
)

 # sidebar elements for the visualization ---------------------------------
sidebar <- dashboardSidebar(

  uiOutput("serverList"),
  sidebarMenu(

    menuItem("MariaDB Status", tabName = "dashboard", icon = icon("dashboard")
    ),

    htmlOutput("menuNote"
    ),

    menuItem("Statement Analysis", tabName = "statementAnalysisTab", icon = icon("table")
    ),

    menuItem("Index Statistics", tabName = "indexData", icon = icon("book")
    ),

    menuItem("User Statistics", tabName = "userStatTab", icon = icon("user")
    ),

    menuItem("Events", tabName = "eventData", icon = icon("clock-o")
    ),

    menuItem("InnoDB Status Output", tabName = "innoDBStatus", icon = icon("file-o")
    ),

    menuItem("Allocated Mem", tabName = "memStat", icon = icon("info")
    ),

    menuItem("Server Variables", tabName = "variables", icon = icon("globe")
    ),

    menuItem("MaxScale", tabName = "maxScale", icon = icon("sitemap")
    ),

    menuItem("Configuration", tabName = "config", icon = icon("cog")
    )
  ) # /sidebarMenu
)

# body elements for the visualization ---------------------------------
body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tabItems(
    tabItem(tabName = "dashboard",
            h2("MariaDB Status"),

            fluidRow(
              tabBox(

                title = tagList(shiny::icon("user"), "Timeline"), width = 12,
                tabPanel("Processes",
                         dygraphOutput("dygraphProcess")
                ),
                tabPanel("Log writes per second",
                         dygraphOutput("dygraphLogWrite")
                ),
                tabPanel("InnoDB buffer pool reads/writes per second",
                         dygraphOutput("dygraphBufferReads")
                )
              )
            ),

            fluidRow(
              box(title = textOutput("buffTot"), htmlOutput("plotBufferFree"), background = "light-blue", width = 3),
              box(title = "InnoDB Buffer Pool Hitrate", htmlOutput("plotBufferHitrate"), background = "light-blue", width = 3),
              box(title = "Table Cache Hitrate", htmlOutput("plotTableCacheHitrate"), background = "light-blue", width = 3),
              box(title = "Temp Tables on Disk", htmlOutput("plotTempTables"), background = "light-blue", width = 3,
                  actionLink("panelTmpTbl", "> View temporary table statements", href = "#shiny-tab-statementAnalysisTab",
                             "data-toggle" = "tab", style = "color:#FFF;")
              )
            ),

            fluidRow(
              tabBox(

                title = tagList(shiny::icon("user"), "Process List"), width = 12,
                tabPanel("Active",
                         tableOutput("tblProcessActive")
                ),
                tabPanel("Sleep",
                         tableOutput("tblProcessSleep")
                )
              )
            ),

            column(12,
                   actionButton("moreInfo", "More..", style = "margin-bottom:18px;"
            )),

            conditionalPanel("input.moreInfo == 1",

                fluidRow(
                  box(title = "Temp Tables", DT::dataTableOutput("tblTmpTables"), width = 6),
                  box(title = "Table Locking", DT::dataTableOutput("tblTblLock"), width = 6)
                ),

                fluidRow(
                  box(title = "Slow Queries", DT::dataTableOutput("tblSlowQry"), width = 4),
                  box(title = "Used Connections", DT::dataTableOutput("tblUsedCon"), width = 4),
                  box(title = "Worker Threads", DT::dataTableOutput("tblWorkThread"), width = 4)
                ),

                fluidRow(
                  box(title = "Key Buffer Size", DT::dataTableOutput("tblKeyBuffSize"), width = 4),
                  box(title = "Query Cache", DT::dataTableOutput("tblQryCache"), width = 4),
                  box(title = "Sort Operations", DT::dataTableOutput("tblSortOp"), width = 4)
                ),

                fluidRow(
                  box(title = "Joins", DT::dataTableOutput("tblJoins"), width = 4),
                  box(title = "Table Scans", DT::dataTableOutput("tblTblScan"), width = 4),
                  box(title = "Binlog Cache", DT::dataTableOutput("tblBinlogCache"), width = 4)
                )
            )
    ), # /tabItem

    # Tab with Statement Analysis-Data
    tabItem(tabName = "statementAnalysisTab",
            p(includeMarkdown("./assets/statements.md")),

            fluidRow(
              tabBox(

                id = "panelStatements",
                title = tagList(shiny::icon("table"), "Statement Analysis (Top 50)"), width = 12,
                tabPanel("By execution frequency",
                         DT::dataTableOutput("tblExecFreq")
                ),
                tabPanel("By runtimes in 95th percentile",
                         DT::dataTableOutput("tblRun95thPerc")
                ),
                tabPanel("Statements with full table scan",
                         DT::dataTableOutput("tblFullTableScan")
                ),
                tabPanel("Statements with temp disc tables",
                         DT::dataTableOutput("tblTmpDiscTableStmt")
                ),
                tabPanel("Statements with warnings and errors",
                         DT::dataTableOutput("tblErrWarnData")
                )
              )
            )
    ),

    # Tab with User-Statistic
    tabItem(tabName = "userStatTab",
            p(includeMarkdown("./assets/user.md")),

            fluidRow(
              tabBox(

                id = "panelUsers",
                title = tagList(shiny::icon("table"), "User and Host Statistic"), width = 12,
                tabPanel("User Statistc",
                         DT::dataTableOutput("tableUserStat")
                ),
                tabPanel("Host Statistic",
                         DT::dataTableOutput("tblHostCache")
                )
              )
            )
    ),

    # Tab with allocated Mem-Info
    tabItem(tabName = "memStat",

            fluidRow(
              box(title = "Global allocated memory", tableOutput("tblMemStatGlob"), width = 12)
            ),

            h4(textOutput("maxCon")
            ),

            fluidRow(
              box(title = "Per Thread allocated memory", tableOutput("tblMemStatThread"), width = 12)
            )
    ),

    # Tab with event Data
    tabItem(tabName = "eventData",
            p(includeMarkdown("./assets/event.md")),

            fluidRow(
              box(title = "Event Statistics", tableOutput("tblEventStat"), width = 12)
            )
    ),

    # Tab with Index Data
    tabItem(tabName = "indexData",
            p(includeMarkdown("./assets/unusedIndexes.md")),

            fluidRow(
              tabBox(
                tabPanel(title = "Unused Indexes", DT::dataTableOutput("tblUsedIndex")),
                tabPanel(title = "Index Cardinality", DT::dataTableOutput("tblindexCardinality")),
                tabPanel(title = "Nullable Indexes", DT::dataTableOutput("tblindexNullable"))
              )
            )
    ),

    # InnoDB Status
    tabItem(tabName = "innoDBStatus",

            fluidRow(
              htmlOutput("innoDBStatus")
            )
    ),

    # Variables
    tabItem(tabName = "variables",

            fluidRow(
              box(title = "Server variables", DT::dataTableOutput("tblVariables"), width = 12)
            )
    ),

    # MaxScale
    tabItem(tabName = "maxScale",
            p(includeMarkdown("./assets/maxScale.md")),

            fluidRow(
              uiOutput("maxScaleTabs")
            )
    ),

    # Configuration
    tabItem(tabName = "config",

            fluidRow(
              box(title = "App Configuration", rHandsontableOutput("appVariables", width = "100%", height = "100%"))
            )
    )

  ) # /tabItems
) # /dashboardBody

ui <- fluidPage(dashboardPage(header, sidebar, body, skin = "black"))
