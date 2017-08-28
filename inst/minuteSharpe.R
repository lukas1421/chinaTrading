library(data.table)
library(lubridate)
library(chinaTrading)


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

#get day sharpe with data given in an env
getDaySharpeFromEnv <- function(symb,dat,env) {
  d <- get("d", envir = env)
  #d[, chg:= C/shift(C,1)-1]
  #d[1, chg:= (C/O)-1]

  d<-d[D==dat]
  amD <- d[T<1200]
  pmD <- d[T>1259]

  res <- mean(d[, chg])/sd(d[,chg])*sqrt(240)
  amRes <- mean(amD[, chg])/sd(amD[,chg])*sqrt(240)
  pmRes <- mean(pmD[, chg])/sd(pmD[,chg])*sqrt(240)
  #print(res)
  list(res=res, am=amRes, pm=pmRes)
}

# get sharpe from date (inclusive)
getDaySharpeFromDate <- function(symb, dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:=(C/O)-1]

  env <- new.env()
  assign('d', d, envir = env)

  d<-d[D>=dat, ]
  days <- data.table(days=unique(d[,(D) ]))
  days<-(days[, getDaySharpeFromEnv(symb, days, env), keyby=list(days) ])
  days[, w:=wday(days)-1]
  print(days)
  days[, list(meanRes=mean(res), meanAm=mean(am), meanPm=mean(pm)), keyby=list(w)]

}


#wtd cumulative sharpe.
getWtdCumuSharpe <- function(symb, dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d <- d[D>=dat,]
  #d[1, chg:=(C/O)-1]

  #d[, cumChg:= cumsum(chg)]
  #d[, rowN := .I]
  #d[, meanSoFar:= cumChg/rowN]
  d[, mean:= (cumsum(chg)/.I)]
  d[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  d[, sharpe:= mean/sd*sqrt(240) ]
  print(d)
  return(d)
}

getDayCumuSharpe <- function(symb, dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d <- d[D==dat,]
  d[, mean:= (cumsum(chg)/.I)]
  d[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  d[, sharpe:= mean/sd*sqrt(240) ]
  #print(symb)
  #print(d)
  return(d)
}

# all index
getIndexDaySharpe <- function(dat) {
  env <- new.env()
  indx <- c("sh000001","sz399006","sz399001","sh000016","sh000300")
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


#week to date sharpe
mon <- lubridate::ymd("2017-8-21")
d <- chinaTrading::getDataPureD("sh601398")
d <- d[D>=mon]
d[, chg:= C/shift(C,1)-1]
d[1, chg:= C/O-1]
m <- mean(d[,chg])*240
s <- sd(d[,chg ])*sqrt(240)


#given a date and compute sharpe

getSDWithR <- function(x) {
  len <- length(x)
  sqrt((sum(x^2)/len-(mean(x))^2)*len/(len-1))
}

#relative sharpe strength
dat <- ymd("2017-8-24")
env <- new.env()
indx <- c("sh000001","sz399006","sz399001","sh000016","sh000300")



#gen sumRet and sumRetSq and N file
# d is the date, get all dates before this date and after monday


d <- ymd("2017-8-28")
mon <- getMonOfWeek(d)


# export to file to be processed for wtd sharpe
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

#provide sum and sum sq data for wtd sharpe ( to be used in conjunction with pcb)
getSumSumSqAll <- function(dat) {
  d <- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d <- d[, c(V2,getSumSumSq(V1,dat)), keyby=list(V1)]
  d
}

write.table(d, paste0(getTradingFolder(),"wtdSumSumSq.txt"),quote = FALSE,sep = "\t")


getMonOfWeek <- function(d) {
  w <- lubridate::wday(d)-1
  d-(w-1)
}




