library(chinaTrading)
library(lubridate)
library(data.table)

cutoff <- ymd("2017-8-13")
#weekend task

#compute highest returners



#compute highest sharpe (close to close sharpe)


#compute minute sharpe (minute to minute sharpe)


#compute opc sharpe.
d <- getDataPure("sh510050")

dm <- getDataPureD("sh510050")
dm <- dm[D > cutoff]
dm[, cc:= (C/shift(C,1))-1, ]
dm[is.na(cc), cc:=0]
sd(dm[,cc])*sqrt(240) # this is daily sd
mean(dm[,cc])*240 # this is daily mean
(mean(dm[,cc])*240)/(sd(dm[,cc])*sqrt(240))

getMinuteSharpe <- function(symb,dateCutoff) {
  print(symb)
  dm <- getDataPureD(symb)
  dm <- dm[D > dateCutoff]
  dm[, cc:= (C/shift(C,1))-1, ]
  dm[is.na(cc), cc:=0]
  #sd(dm[,cc])*sqrt(240) # this is daily sd
  #mean(dm[,cc])*240 # this is daily mean
  (mean(dm[,cc])*240)/(sd(dm[,cc])*sqrt(240))
}

getMinuteSharpeAll <- function(dateCutOff) {
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,getMinuteSharpe(V1, dateCutOff)), keyby=list(V1)]
  return(d)
}

#worst monthly return since inception




#worst monthly drawdown


#worst continous drawdown
getWorstMonthReturn <- function(symb) {
  d <- getDataPure(symb)
  d[, mo:= month(D)]
  d<-d[mo!=shift(mo,1,type="lead") ]
  d[, monRet:= (C/shift(C,1))-1]
  list(ret=min(d$monRet,na.rm=T),mo=d[monRet==min(d$monRet,na.rm = T),D])
}






