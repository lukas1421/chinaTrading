

#' get the sharpe ratio of the day given a stock and a date
#' @export
getDaySharpe <- function(symb,dat) {
  d <- chinaTrading::getDataPureD(symb)
  d <- d[D==dat]
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:= (C/O)-1]
  amD <- d[T<1200]
  pmD <- d[T>1259]

  res <- mean(d[, chg])/sd(d[,chg])*sqrt(240)
  amRes <- mean(amD[, chg])/sd(amD[,chg])*sqrt(240)
  pmRes <- mean(pmD[, chg])/sd(pmD[,chg])*sqrt(240)
  list(res=res, am=amRes, pm=pmRes)
}

#' export to file to be processed for wtd sharpe
#' @export
getSumSumSq <- function(symb, dat) {
  print(symb)
  mon <- getMonOfWeek(dat)
  d <- getDataPureD(symb)
  d <- d[D>=mon & D<=dat, ]
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:= C/O-1]
  d[, chgSq:= chg^2]
  #print(d)
  if(nrow(d)!=0){
    sumRet <- d[, sum(chg,na.rm = T)]
    sumRetSq <- d[,sum(chgSq,na.rm = T)]
    n <- d[,.N]
    m <- sumRet/n
    sd <- sqrt((sumRetSq/n - m^2)*n/(n-1))
    sr <- m/sd*sqrt(n)
    #print((list(sumRet=sumRet, sumRetSq=sumRetSq,N=n, mean=m, sd=sd, sr=sr)))
    return(list(sumRet=sumRet, sumRetSq=sumRetSq,N=n, sr=sr))
  }
  return()
}

#' get all sum and sum sq for a given date
#' provide sum and sum sq data for wtd sharpe ( to be used in conjunction with pricemapbar)
#' @export
getSumSumSqAll <- function(dat) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d <- d[, c(V2,getSumSumSq(V1,dat)), keyby=list(V1)]
  names(d) <-c("Ticker", "chn","sumRet","sumRetSq","N","SR")
  write.table(d, paste0(getTradingFolder(),"wtdSumSumSq.txt"),quote = FALSE,sep = "\t"
              ,row.names = FALSE, col.names = FALSE)
  d
}

#' get the monday of a given date
#' @export
getMonOfWeek <- function(d) {
  w <- lubridate::wday(d)-1
  d-(w-1)
}
