#open pnl


#' get open position list
#' @export
getOpenPosPure <- function(dat) {
  tr <- getTradingHistory()
  tr[D<(dat), list(FullTicker,sum(Volume)),
     keyby=list(FullTicker)][V2!=0,][,list(ticker=FullTicker,open=V2),keyby=list(FullTicker)]
}

#' get open position with ytd close price
#' get open pos
#' @export
getOpenPos <- function(dat) {
  tr <- getTradingHistory()
  tr[D<(dat), list(FullTicker,sum(Volume)),
     keyby=list(FullTicker)][V2!=0,][,list(ticker=FullTicker,open=V2,prev=getClosingPriceBeforeD(dat,FullTicker)),
                                     keyby=list(FullTicker)]
}


#' open pnl
getOpenPnl <- function(dat, symb, pos) {
  tryCatch ({
    ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    stock <- fread(paste0(getDayDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
    stock <- stock [!.N,]
    stock[, D:=ymd(D)]
    open <- stock[D==dat, O]
    prevClose <- stock[D<dat,][.N,C]
    #print(paste("open ",open, "prevoius close ", prevClose))
    if(!is.na(open) & !is.na(prevClose)) {
      return((open-prevClose)*pos)
    } else {
      0
    }
  }, error = function(err) {
    print(err)
    return(0.0)
  })
}

#' open pnl for all stocks
#'
getOpenPnlForPtf <- function(dat) {
  openPos <- getOpenPosPure(dat)
  openPos[, openPnl:= getOpenPnl(dat,ticker,open), keyby=list(ticker)]
  print(openPos)
  openPos[, sum(openPnl,na.rm=T)]
}
