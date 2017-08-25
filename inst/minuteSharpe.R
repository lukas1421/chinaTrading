library(data.table)
library(lubridate)
library(chinaTrading)


getDaySharpe <- function(symb,dat) {
  d <- chinaTrading::getDataPureD(symb)
  d[, chg:= C/shift(C,1)-1]
  d[1, chg:= (C/O)-1]
  d<-d[D==dat]
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


