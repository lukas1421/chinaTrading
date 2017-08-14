#trades analysis



###########################################################methods ##########################################
calcOpenPositionForStock <- function(dat, symb)  {
  tr <- getTradingHistory()
  res <- tr[FullTicker==toupper(symb) & D<dat, list(sum=sum(Volume)),keyby=list(FullTicker)][,sum]
  #print(paste(dat, symb, res))
  return(ifelse(length(res)==0,0.0,res))
}

getStockPositionHistory <- function(symb) {
  tradesDates <- getTradingDates()
  res<-tradeDates[, list(D, pos = as.double(calcOpenPositionForStock(D,symb))), keyby=list(D)]
  res[pos!=0]
}

daysWithActivePosition <- function(symb){
  res <- getStockPositionHistory(symb)
  nrow(res)
}

loadRecurrentStocks <- function() {
  recurList <- stockActiveDays[V1>=1][, (FullTicker)]
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
