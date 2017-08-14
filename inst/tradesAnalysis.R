#load trades into R

library(chinaTrading)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)


getTradingHistory <- function() {
  tr <- fread(paste0(getTradingFolder(),"trades.csv"))
  tr[, D:=ymd(Date)]
  tr[, w:= wday(D)-1]
}

getTradingDates <- function() {
  tradeDates <- fread(paste0(getTradingFolder(),"tradeDates.csv"))
  tradeDates[, D:=ymd(Date)]
  tradeDates[, w:= wday(D)-1]
}



stockActiveDays <- tr[ ,daysWithActivePosition(FullTicker), list(FullTicker)]

#check top/worst performer
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(-V1)][1:20]
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(V1)][1:20]

#open position for a given date
tr[D< ymd("2017-8-11"), list(FullTicker,sum(Volume)), keyby=list(FullTicker)][V2!=0]


dat <-ymd("2017-8-11")
d<-tr[D< dat, list(FullTicker,sum(Volume),getClosingPriceBeforeD(dat,FullTicker)), keyby=list(FullTicker)][V2!=0]


# CAUTION LONG EXECUTION #############
yrMtm<-tradeDates[D>=ymd("2017-4-30"),getMTMForAll(D),keyby=list(D)]
############################################

yrMtm[, list(fullSum=sum(Full,na.rm = T),amSum=sum(AM,na.rm = T),pmSum=sum(PM,na.rm = T)),]


#save
write.table(yrMtm, paste0(getTradingFolder(),"ampmmtm.txt"),quote = FALSE,sep = "\t",row.names = F)
yrMtmTest <- fread(paste0(getTradingFolder(),"ampmmtm.txt"))

yrMtm[, list(sum(Full,na.rm = T),sum(AM,na.rm = T),sum(PM,na.rm = T))]
yrMtm[, list(mean(Full,na.rm = T),mean(AM,na.rm = T),mean(PM,na.rm = T))]



#get max min info
maxMin<-tr[D>ymd("2017-8-6"),getMinuteMtmForAll(D),keyby=list(D)]

##########################FUNCTIONS########################################################################################################


#######open pnl
#open pnl
openPnl <- tr[D>ymd("2017-4-30"),list(openPnl=getOpenPnlForPtf(D)), keyby=list(D)]
openPnl[,w:=wday(D)-1]




###am pm
# trades analysis am pm analysis
maxMin[, dayMaxT1:= convertTimeToDecimal(dayMaxT)]
maxMin[, dayMinT1:= convertTimeToDecimal(dayMinT)]
maxMin[, amMaxT1:= convertTimeToDecimal(amMaxT)]
maxMin[, amMinT1:= convertTimeToDecimal(amMinT)]
maxMin[, pmMaxT1:= convertTimeToDecimal(pmMaxT)]
maxMin[, pmMinT1:= convertTimeToDecimal(pmMinT)]

#get max/min time mean
maxMin[, list(daymax=mean(dayMaxT1), daymin=mean(dayMinT1), ammax = mean(amMaxT1), ammin = mean(amMinT1), pmmax=mean(pmMaxT1),pmmin=mean(pmMinT1)), keyby=list(w)]

#range
maxMin[, amRange:= amMax - amMin]
maxMin[, pmRange:= pmMax - pmMin]

maxMin[, mean(amRange), keyby=list(w)]
maxMin[, mean(pmRange), keyby=list(w)]

#merge
fullMtmMaxMin<-merge(maxMin, yrMtm, by = "D")

#look at morning sharpe (return/sd(range))
fullMtmMaxMin[ , list(AM=mean(AM), AMRange= mean(amRange), rangeSD= sd(amRange), amSharpe = mean(AM)/sd(amRange)),, keyby=list(w.x)]
fullMtmMaxMin[ , list(PM=mean(PM), PMRange= mean(pmRange), rangeSD= sd(pmRange), pmSharpe = mean(PM)/sd(pmRange)),, keyby=list(w.x)]




computeOpenDelta <- function(dat){
  d<-tr[D< (dat), list(FullTicker,sum(Volume)),
        keyby=list(FullTicker)][V2!=0,][,list(FullTicker, V2,getClosingPriceBeforeD(dat,FullTicker)),keyby=list(FullTicker)]
  d[, sum(V2*V3)]
}

