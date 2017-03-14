#' load app configuration (schedules, thresholds)
#'
#' ...
#'
#' @export
appConfig <- read.csv2(paste0(system.file("app", package = "INWTdbMonitor"), "/app.cnf"),
                       stringsAsFactors = FALSE, header = TRUE) %>% mutate(value = as.numeric(value))

## general app-functions ---------------------------------

#' paste function space
#'
#' ...
#'
#' @export
`%p%`        <- function(x, y) paste(x, y)

#' paste function not space
#'
#' ...
#'
#' @export
`%p0%`       <- function(x, y) paste0(x, y)

## general functions ---------------------------------

#' funtion to get app configuration value
#'
#' ...
#'
#' @export
configVal <- function(x) {

  tmp <- appConfig %>% filter(appVariable == x) %>% select(2)

  return(tmp[1, 1])

}

#' funtion to get server variable value
#'
#' ...
#'
#' @export
serverVal <- function(dat, name) {
  dat %>% filter(VARIABLE_NAME %in% name) %>% select(2) %>% t %>% as.vector
}

#' funtion to get server variable numeric value
#'
#' ...
#'
#' @export
serverValNum <- function(dat, name) {
  serverVal(dat, name) %>% as.numeric
}

#' This function takes a number and returns a compressed string (e.g. 1624 => 1.6K or 2K, depending on round.by)
#'
#' ...
#'
#' @export
compress <- function(x, round.by = 2) {
  # by StackOverflow user 'BondedDust' : http://stackoverflow.com/a/28160474
  div <- findInterval(as.numeric(gsub("\\, ", "", x)), c(1, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\, ", "", x)) / 10 ^ (3 * (div - 1)), round.by), c("", "K", "M", "B", "T")[div], sep = "" )
}

#' Calculate Differences in Timeline-Data
#'
#' ...
#'
#' @export
helperBufferWrite <- function(datOld, datNew) {
  tmp <-  rbind(datOld, datNew) %>%
          mutate(VARIABLE_VALUE = as.numeric(VARIABLE_VALUE),
                 DATETIME = as.POSIXct(DATETIME)) %>%
          group_by(VARIABLE_NAME) %>%
          mutate(VARIABLE_VALUE_SEC = (VARIABLE_VALUE - lag(VARIABLE_VALUE)) / (as.numeric(DATETIME - lag(DATETIME)))) %>% data.frame
  return(tmp)
}

#' data handling for dygraph
#'
#' ...
#'
#' @export
helperDygraphDat <- function(.dat) {
  tmp <- .dat %>%
    select(DATETIME, VARIABLE_NAME, VARIABLE_VALUE_SEC) %>%
    spread(VARIABLE_NAME, VARIABLE_VALUE_SEC) %>%
    xts(., order.by = strptime(.$DATETIME, format = "%Y-%m-%d %H:%M:%S"), tzone = appDbTz)
  tmp <- tmp[, -1]
  return(tmp)
}

# function to create config file for database connection
createCnf <- function() {

  if (file.exists("~/.INWTdbMonitor")) {

    if (!file.exists("~/.INWTdbMonitor/cnf.file")) {
      file.copy(paste0(system.file("app", package = "INWTdbMonitor"), "/cnf.file"), "~/.INWTdbMonitor/cnf.file", overwrite = FALSE)
    }

  } else {
    dir.create("~/.INWTdbMonitor")
    file.copy(paste0(system.file("app", package = "INWTdbMonitor"), "/cnf.file"), "~/.INWTdbMonitor/cnf.file", overwrite = FALSE)
  }

  invisible()

}

createCnf()

#' function to enter credentials to database server
#'
#' ...
#'
#' @export
promptCnfData <- function() {

    createCnf()

    appConfig <- readLines("~/.INWTdbMonitor/cnf.file")

    appConfig[2] <- paste0("user=", readline(prompt = "Enter the username for the database connection: "))
    appConfig[3] <- paste0("password=", readline(prompt = "Enter the password for the database connection: "))
    appConfig[5] <- paste0("host=", readline(prompt = "Enter the database host: "))
    appConfig[6] <- paste0("port=", readline(prompt = "Enter the database port: "))

    writeLines(appConfig, "~/.INWTdbMonitor/cnf.file")

}

# Grep matching Line in cnf.file
grepLine <- function(file, what) {
  gsub(what, "", file[grep(what, file)])
}

#' init database server
#'
#' ...
#'
#' @export
initDbServer <- function() {
  sqlCredFile <- readLines("~/.INWTdbMonitor/cnf.file")
  grepLine(sqlCredFile, "host=") %p0% ":" %p0% grepLine(sqlCredFile, "port=")

}

# reactive function for database credentials
mutateDbConfig <- function() {

  sqlCredFile <- readLines("~/.INWTdbMonitor/cnf.file")

  init <- function() {
    sqlCredFile <<- readLines("~/.INWTdbMonitor/cnf.file")
  }

  get <- function() {
    sqlCredFile
  }

  set <- function(hostPort) {
    sqlCredFile[5] <<- "host=" %p0% strsplit(hostPort, ":")[[1]][1]
    sqlCredFile[6] <<- "port=" %p0% strsplit(hostPort, ":")[[1]][2]
    # sqlCredFile
  }


  list(set = set, get = get, init = init)

}

#' reactive object for database credentials
#'
#' ...
#'
#' @export
dbConfig <- mutateDbConfig()


#' generate SQL-Cedentials
#'
#' ...
#'
#' @export
genSQLCred <- function() {

  Credentials (
    drv = MySQL,
    username = grepLine(dbConfig$get(), "user="),
    password = grepLine(dbConfig$get(), "password="),
    dbname = grepLine(dbConfig$get(), "database="),
    host = grepLine(dbConfig$get(), "host="),
    port = as.numeric(grepLine(dbConfig$get(), "port="))
  )
}

#' function to query mariadb
#'
#' ...
#'
#' @export
queryDB <- function(query, host = dbConfig$get(), port = dbConfig$get()) {

  qry <- query

  dbSendQuery <- function(...) {
    suppressWarnings(RMySQL::dbSendQuery(...))
  }

  con <- dbConnect(MySQL(),
                   username = grepLine(dbConfig$get(), "user="),
                   password = grepLine(dbConfig$get(), "password="),
                   dbname = grepLine(dbConfig$get(), "database="),
                   host = grepLine(host, "host="),
                   port = as.numeric(grepLine(port, "port="))
  )

  tmp <- dbSendQuery(con, qry)
  dat <- dbFetch(tmp, n = -1)
  dbClearResult(tmp)
  dbDisconnect(con)

  return(dat)
}

#' Vars for transformation for thousend (10000 -> 10 000)
#'
#' ...
#'
#' @export
transformThousends <- function(.df) {
  .df %>%
    mutate(VARIABLE_VALUE = ifelse(VARIABLE_NAME %in% varsTransformThousend,
                                   prettyNum(VARIABLE_VALUE, big.mark = " ", decimal.mark = "."),
                                   VARIABLE_VALUE)
           )
}

#' Change href link to href tab-toggle
#'
#' ...
#'
#' @export
linkToTab <- function(link, msg){
  msg$children[[1]] <- a(href = paste0(link), "data-toggle" = "tab", list(msg$children[[1]]$children))
  return(msg)
}

#' string to number -> on error na
#'
#' ...
#'
#' @export
asNum <- function(x) {
  tryCatch(suppressWarnings(as.numeric(x)), error = function(e) NA)
}

#' mutate process data
#'
#' ...
#'
#' @export
procToTimeLine <- function(timeData) {

  # datahandling for calculating tot_connections, tot_memory and run_connections
  timeData <- timeData %>%
    group_by(COMMAND) %>%
    summarise(CONNECTIONS = n(),
              TIME = sum(TIME),
              MEMORY_USED = sum(MEMORY_USED),
              DATETIME = max(DATETIME)) %>%
    data.frame

  timeData <- timeData %>%
    mutate(MEMORY_USED = MEMORY_USED / 1024 / 1024) %>%
    rbind(.,
          c(COMMAND = "Total",
            unlist(c(timeData %>%
                       group_by(DATETIME)  %>%
                       summarise(CONNECTIONS = sum(CONNECTIONS), TIME = sum(TIME), MEMORY_USED = sum(MEMORY_USED / 1024 / 1024)) %>%
                       select(CONNECTIONS, TIME, MEMORY_USED, DATETIME)
            )
            )
          )
    ) %>%
    filter(COMMAND %in% c('Total', 'Query')) %>%
    summarise(DATETIME = max(DATETIME),
              TOT_CONNECTIONS = max(CONNECTIONS),
              TOT_MEMORY = max(MEMORY_USED),
              RUN_CONNECTIONS = min(CONNECTIONS)) %>%
    data.frame

  timeData$DATETIME <- strptime(timeData$DATETIME, format = "%H:%M:%S")
  timeData <- xts(timeData[, -1], order.by = timeData[, 1], tzone = appDbTz)

  return(timeData)

}

#' get flag slave server exist from process data
#'
#' ...
#'
#' @export
procSlaveServer <- function(procList) {

  if (length(grep("Binlog Dump", procList$COMMAND)) == 0) return(NULL)

  procList %>%
    filter(grepl("Binlog Dump", COMMAND)) %>%
    mutate(HOST = strsplit(HOST, ":")[[1]][1] %p0% ":3306")

}

#' get flag maxscale exist from process data
#'
#' ...
#'
#' @export
procMaxscale <- function(procList) {

  if (length(grep("maxscale", procList$USER)) == 0) return(NULL)

  procList %>%
    filter(grepl("maxscale", USER)) %>%
    mutate(HOST = strsplit(HOST, ":")[[1]][1],
           PORT = 9003)

}

#' clean process data
#'
#' ...
#'
#' @export
cleanProcList <- function(procList) {

  procList %>%
    mutate(MEMORY_USED = formatIECBytes(MEMORY_USED),
           TIME = as.character(seconds_to_period(TIME)),
           ID = as.character(ID)) %>%
    select(-DATETIME)

}




