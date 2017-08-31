#trades analysis



###########################################################methods ##########################################
#' calc position stock
#' @export
#' @param dat date
#' @param symb stock
calcOpenPositionForStock <- function(dat, symb)  {
  tr <- getTradingHistory()
  res <- tr[FullTicker==toupper(symb) & D<dat, list(sum=sum(Volume)),keyby=list(FullTicker)][,sum]
  return(ifelse(length(res)==0,0.0,res))
}

getStockPositionHistory <- function(symb) {
  tradeDates <- getTradingDates()
  res<-tradeDates[, list(D, pos = as.double(calcOpenPositionForStock(D,symb))), keyby=list(D)]
  res[pos!=0]
}

#' get days with active position
#' @export
#' @param symb stock
daysWithActivePosition <- function(symb){
  res <- getStockPositionHistory(symb)
  print(paste(symb,nrow(res)))
  nrow(res)
}

#' load into main namespace all stocks that exceed an active day threshold of 1
#' @export
loadRecurrentStocks <- function() {

  tr <- getTradingHistory()

  recurList<-tr[, list(sum=sum(abs(Volume))), keyby=list(FullTicker)][sum>0,(FullTicker)]

  #stockActiveDays <- tr[ ,daysWithActivePosition(FullTicker), list(FullTicker)]
  #tr[, list(sum=sum(abs(Volume))), keyby=list(FullTicker)][sum>0,]
  #recurList <- stockActiveDays[V1>=1][, (FullTicker)]
  for(s in recurList) {
    print(s)
    data <- getMinuteDataPureForAllDate(s)
    assign(s,data ,envir = .GlobalEnv)
  }
}

# calcOpenPositionForStock <- function(dat, symb)  {
#   res <- tr[FullTicker==toupper(symb) & D<dat, list(sum=sum(Volume)),keyby=list(FullTicker)][,sum]
#   #print(paste(dat, symb, res))
#   return(ifelse(length(res)==0,0.0,res))
# }
#
# getStockPositionHistory <- function(symb) {
#   res<-tradeDates[, list(D, pos = as.double(calcOpenPositionForStock(D,symb))), keyby=list(D)]
#   res[pos!=0]
# }
#
# daysWithActivePosition <- function(symb){
#   res <- getStockPositionHistory(symb)
#   nrow(res)
# }
