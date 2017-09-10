#' get the sharpe ratio of the day given a stock and a date
#' @export
#' @param symb stock
#' @param dat date
getDaySharpe <- function(symb,dat) {
  print(symb)
  d <- chinaTrading::getDataPureD(symb)
  d <- d[D==dat]
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:= (C/O)-1]
  amD <- d[T<1200]
  pmD <- d[T>1259]

  res <- d[, mean(chg)/sd(chg)*sqrt(240)]

  #res <- mean(d[, chg])/sd(d[,chg])*sqrt(240)
  #amRes <- mean(amD[, chg])/sd(amD[,chg])*sqrt(240)
  #pmRes <- mean(pmD[, chg])/sd(pmD[,chg])*sqrt(240)
  #list(res=res, am=amRes, pm=pmRes)
  list(res=res)
}

#' m/t/w/th/fr sharp
#' @export
#' @param symb stock
#' @param dat date
getWtdDailySharpe <- function(symb,dat) {
  print(symb)
  mon <- getMonOfWeek(dat)

  d <- chinaTrading::getDataPureD(symb)
  d <- d[D>=mon,]
  d[, chg:= C/shift(C,1)-1]
  l <- unique(d$D)
  env <- new.env(parent = emptyenv())
  res <- new.env(parent = emptyenv())
  assign("d",d,envir = env)
  lapply(l, function(x) {
        assign(paste0(x,""),getDaySharpeFromEnv(sym,ymd(x),env),envir = res)
    })
  as.list(res)
}

#' get wtd sharpe all
#' @export
#' @param dat date
getWtdDailySharpeAll <- function(dat) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d <- d[, c(V2,getWtdDailySharpe(V1,dat)), keyby=list(V1)]
  d
}


#' get cumulative sharpe of one day for one symbol
#' @export
#' @param symb stock
#' @param dat date
getDayCumuSharpe <- function(symb, dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d <- d[D==dat,]
  # d[, mean:= (cumsum(chg)/.I)]
  # d[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  # d[, sharpe:= mean/sd*sqrt(240) ]
  #print(symb)
  #print(d)
  return(cbind(d[,list(D,O,C)],d[,getDayCumSharpeCpp(chg),]))
}

#' export to file to be processed for wtd sharpe
#' @export
#' @param symb stock
#' @param dat date
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
    return(d[!is.na(chg), getSumChgC(chg)])
  }
  return()
}

#' get all sum and sum sq for a given date
#' provide sum and sum sq data for wtd sharpe ( to be used in conjunction with pricemapbar)
#' @export
#' @param dat date
getSumSumSqAll <- function(dat) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d <- d[, c(V2,getSumSumSq(V1,dat)), keyby=list(V1)]
  names(d) <-c("Ticker", "chn","sumRet","sumRetSq","N","SR")
  write.table(d, paste0(getTradingFolder(),"wtdSumSumSq.txt"),quote = FALSE,sep = "\t"
              ,row.names = FALSE, col.names = FALSE)
  d
}

#' wtd cumulative sharpe.
#' @export
#' @param symb stock
#' @param dat date
getWtdCumuSharpe <- function(symb, dat) {
  mon <- getMonOfWeek(dat)
  d <- chinaTrading::getDataPureD(symb)
  d <- d[D>=mon,]
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:=(C/O)-1]
  #d[, cumChg:= cumsum(chg)]
  #d[, rowN := .I]
  #d[, meanSoFar:= cumChg/rowN]

  d[, mean:= (cumsum(chg)/.I)]
  d[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  d[, sharpe:= mean/sd*sqrt(240) ]
  print(d)
  return(d)
}

#' the last function was cumulative (showing sharpe for each line)
#' this method only gets the end of week sharpe
#' @export
#' @param symb stock name
#' @param dat date
getWtdSharpe <- function(symb, dat) {
  print(symb)
  mon <- getMonOfWeek(dat)
  d <- chinaTrading::getDataPureD(symb)
  d <- d[D>=mon,]
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:=(C/O)-1]
  list(wtdSr=d[,mean(chg)/sd(chg)*sqrt(240)])
  #print(d[, mean(chg)/sqrt((mean(chg^2)-(mean(chg))^2)*.N/(.N-1))*sqrt(240)])
}

#' apply the same function to all stocks in test.txt
#' @export
#' @param f function to apply
#' @param ... any other parameters
applyFunctionToAllStocks <- function(f,...) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d <- d[1:10, c(V2,f(V1,...)), keyby=list(V1)]
  d
}




#' get minute cumulative sharpe
#' @export
#' @param dat date
getIndexDayCumuSharpe <- function(dat) {
  env <- new.env()
  indx <- c("sh000001","sz399006","sz399001","sh000016","sh000300","sh000905")
  env$res <- data.table()
  sapply(indx,
         function(x) {
           d <- getDayCumuSharpe(x,dat)
           d<-d[, list(T,sharpe)]
           names(d) <- c("T", x)
           print(d)
           assign(x, d, env = env)
           if(nrow(env$res)==0) {
             env$res <- d
           } else{
             env$res <- merge(env$res, d, by.x = "T", by.y="T" )
           }
         })
  return(env$res)

  # d <- chinaTrading::getDataPureD(symb)
  # d[, chg:= C/shift(C,1)-1]
  # d <- d[D==dat,]
  # d[, mean:= (cumsum(chg)/.I)]
  # d[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  # d[, sharpe:= mean/sd*sqrt(240) ]
  # print(d)
  # return(d)
  #invisible()
}

#' get the monday of a given date
#' @param d a date
getMonOfWeek <- function(d) {
  w <- data.table::wday(d)-1
  d-(w-1)-ifelse(w==0,7,0)
}

#' get day sharpe with data given in an env
#' @param symb stock
#' @param dat date
#' @param env environment from which to getdata
getDaySharpeFromEnv <- function(symb,dat,env) {
  d <- get("d", envir = env)
  #d[, chg:= C/shift(C,1)-1]
  #d[1, chg:= (C/O)-1]

  d<-d[D==dat]
  d[1, chg:=(C/O)-1]
  # amD <- d[T<1200]
  # pmD <- d[T>1259]
  res <- mean(d[, chg])/sd(d[,chg])*sqrt(240)
  # amRes <- mean(amD[, chg])/sd(amD[,chg])*sqrt(240)
  # pmRes <- mean(pmD[, chg])/sd(pmD[,chg])*sqrt(240)
  #print(res)
  #list(res=res, am=amRes, pm=pmRes)
  res
}

#' get sharpe from date (inclusive)
#' @export
#' @param symb stock
#' @param dat date
getDaySharpeSinceDate <- function(symb, dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:=(C/O)-1]

  env <- new.env()
  assign('d', d, envir = env)

  d<-d[D>=dat, ]
  days <- data.table(days=unique(d[,(D) ]))
  days <- (days[, getDaySharpeFromEnv(symb, days, env), keyby=list(days) ])
  days[, w:=wday(days)-1]
  print(days)
  days[, list(meanRes=mean(res), meanAm=mean(am), meanPm=mean(pm)), keyby=list(w)]

}

#' get the list of high sharpe stocks on a given date
#' @export
#' @param dat date
getSharpeAllStocksOnDat <- function(dat) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  # getDaySharpe
  d <- d[, c(V2, getDaySharpe(V1,dat)), keyby=list(V1)]
  print(d)
  d
}

