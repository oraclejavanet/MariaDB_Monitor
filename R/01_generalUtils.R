## general app-functions ---------------------------------

#' paste functions
#'
#' ...
#'
#' @export
`%p%`        <- function(x, y) paste(x, y)
`%p0%`       <- function(x, y) paste0(x, y)

#' load app configuration (schedules, thresholds)
#'
#' ...
#'
#' @export
appConfig <- read.csv2(system.file("app", package = "INWTdbMonitor") %p0%
                         "/app.cnf", stringsAsFactors = FALSE, header = TRUE) %>% mutate(value = as.numeric(value))

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

# Conditional icon for widget.
# Returns arrow-up icon on true (if true_direction is 'up'), e.g. load time % change > 0
cond_icon <- function(condition, true_direction = "up") {

  if (true_direction == "up") {
    return(icon(ifelse(condition, "arrow-up", "arrow-down")))
  }

  return(icon(ifelse(condition, "arrow-down", "arrow-up")))
}

# Conditional color for widget
# Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
#                                               load time % change < 0
cond_color <- function(condition, true_color = "green") {
  if (is.na(condition)){
    return("black")
  }

  colours <- c("green", "red")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
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
  sqlCredFile <- readLines(system.file("app", package = "INWTdbMonitor") %p0% "/cnf.file")
  grepLine(sqlCredFile, "host=") %p0% ":" %p0% grepLine(sqlCredFile, "port=")

}

# reactive function for database credentials
mutateDbConfig <- function() {

  sqlCredFile <- readLines(system.file("app", package = "INWTdbMonitor") %p0% "/cnf.file")

  get <- function() {
    sqlCredFile
  }

  set <- function(hostPort) {
    sqlCredFile[5] <<- "host=" %p0% strsplit(hostPort, ":")[[1]][1]
    sqlCredFile[6] <<- "port=" %p0% strsplit(hostPort, ":")[[1]][2]
    # sqlCredFile
  }


  list(set = set, get = get)

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
