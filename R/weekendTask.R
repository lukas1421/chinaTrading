#'computing week percentile and week return
#'@export
#'@param symb stock
computeWeekPercReturn <- function(symb) {
  m<- getMonOfWeek(Sys.Date())
  d <- getDataPure(symb)
  friClose <-  d[D<m][.N,C]
  monOpen <- d[D>=m][1, O]
  d <- d[D>=m]
  # high <- max(d$H)
  # low <- min(d$L)
  # last <- d[.N,C]
  percentile <- getPercentileCpp(d)
  lastClose <- d[.N,C]
  return(list(percentile=percentile , weekreturn=(lastClose/friClose-1)))
}

#'compute week return and percentile for all stocks
#'@export
computeWeekendAll <- function() {
  res<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  res<- res[,computeWeekPercReturn(V1),keyby=list(V1)]
  res
}

#' compute cumulative wtd sharpe
#' @export
m1 <- function(symb) {
  m<- getMonOfWeek(Sys.Date())
  d <- getDataPure(symb)
  d<- d[D>=m]
  return(d)

}


getPriceForDate <- function(symb, dat) {
  d <- getDataPure(symb)
  res <- d[D==dat, C]
  return(list(price=res))
}


getPriceForDateAll <- function() {
  res<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  res<- res[,(getPriceForDate(V1,ymd("20170526"))),keyby=list(V1)]
  res
}

# write.table(d, "C:\\Users\\LUke\\Desktop\\Trading\\may26Close.txt"
#             ,quote = FALSE,sep = "\t", row.names = FALSE)



#weekend percentile all
