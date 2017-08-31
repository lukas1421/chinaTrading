library(data.table)
library(lubridate)
library(chinaTrading)




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



# all index
getIndexDaySharpe <- function(dat) {
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

#given a date and compute sharpe
getSDWithR <- function(x) {
  len <- length(x)
  sqrt((sum(x^2)/len-(mean(x))^2)*len/(len-1))
}

############################################################################################################
#script
#week to date sharpe
mon <- lubridate::ymd("2017-8-21")
d <- chinaTrading::getDataPureD("sh601398")
d <- d[D>=mon]
d[, chg:= C/shift(C,1)-1]
d[1, chg:= C/O-1]
m <- mean(d[,chg])*240
s <- sd(d[,chg ])*sqrt(240)


#relative sharpe strength
dat <- ymd("2017-8-24")
env <- new.env()
indx <- c("sh000001","sz399006","sz399001","sh000016","sh000300")



#gen sumRet and sumRetSq and N file
# d is the date, get all dates before this date and after monday
d <- ymd("2017-8-28")
mon <- getMonOfWeek(d)







