library(data.table)
library(lubridate)
library(chinaTrading)



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







