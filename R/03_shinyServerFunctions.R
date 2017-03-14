# functions for shiny server -----------------------------

#' Table config
#'
#' ...
#'
#' @export
appDataTable <- function(x) {
  datatable(x,
            options = list(pageLength = 50, searching = FALSE, paging = FALSE, info = FALSE, processing = FALSE))
}

#' Piechart
#'
#' ...
#'
#' @export
appPieChart <- function(label, value, badColor = "#FF9800", goodColor = "#4CAF50") {

  Sys.sleep(0.3) #http://stackoverflow.com/questions/33530066/shiny-googlevis-geochart-not-displaying-with-reactive-switch

  gvisPieChart(
    data.frame(label = label,
               value = value
    ),
    options = list(
      colors = "['" %p0% badColor %p0% "', '" %p0% goodColor %p0% "']",
      sliceVisibilityThreshold = 0,
      pieHole = 0.1)
  )
}

#' reload data by schedule
#'
#' ...
#'
#' @export
reloadBySchedule <- function(dat, schedule, session = getDefaultReactiveDomain()) {
  invalidateLater(configVal(schedule), session)
  dat
}

#' dataset with buffer statistic
#'
#' ...
#'
#' @export
bufferDat <- function (dat) {
  data.frame(
    engine = c("TokuDB", "InnoDB"),
    Filled = c(serverValNum(dat, c("tokudb_cachetable_size_current")) / serverValNum(dat, c("tokudb_cache_size")),
               1 - (serverValNum(dat, c("innodb_buffer_pool_pages_free")) / serverValNum(dat, c("innodb_buffer_pool_pages_total"))))) %>%
    mutate(Free = 1 - Filled)
}

#' total buffer innodb + tokudb
#'
#' ...
#'
#' @export
bufferTotDat <- function (dat) {
  sum(serverValNum(dat, c("tokudb_cachetable_size_current")) / serverValNum(dat, c("tokudb_cache_size")) *
        serverValNum(dat, "tokudb_cache_size"),
      (1 - (serverValNum(dat, c("innodb_buffer_pool_pages_free")) / serverValNum(dat, c("innodb_buffer_pool_pages_total")))) *
        serverValNum(dat, "KPI_bufPoolSize")
      )
}
