library(data.table)
library(lubridate)

d <- getDataPureD("sh601398")
d<-d[D==ymd("2017-8-24")]
d[, chg:= C/shift(C,1)-1]
d[1, chg:= (C/O)-1]
mean(d[,chg])*240
sd(d[,chg])*sqrt(240)
mean(d[, chg])/sd(d[,chg])*sqrt(240)


