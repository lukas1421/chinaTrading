#load trades into R

library(chinaTrading)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)




# getTradingHistory <- function() {
#   tr <- fread(paste0(getTradingFolder(),"trades.csv"))
#   tr[, D:=ymd(Date)]
#   tr[, w:= wday(D)-1]
# }
#
# getTradingDates <- function() {
#   tradeDates <- fread(paste0(getTradingFolder(),"tradeDates.csv"))
#   tradeDates[, D:=ymd(Date)]
#   tradeDates[, w:= wday(D)-1]
# }

loadRecurrentStocks()

tr <- getTradingHistory()
tradeDates <- getTradingDates()

#check top/worst performer
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(-V1)][1:20]
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(V1)][1:20]

tr[, FullTicker:= paste0(Exchage, str_pad(Ticker,6,side = "left", pad="0"))]

#open position for a given date
tr[D< ymd("2017-12-2"), list(FullTicker,sum(Volume)), keyby=list(FullTicker)][V2!=0]
d<-tr[D< ymd("2017-12-2"), list(FullTicker,sum(Volume),getClosingPriceBeforeD(Sys.Date(),FullTicker)), keyby=list(FullTicker)][V2!=0]


# MTM
yrMtm<-tradeDates[D>=ymd("2017-11-27"),getMTMForAll(D),keyby=list(D)]
yrMtm[, w:= wday(D)-1]
yrMtm[, list(fullSum=sum(Full,na.rm = T),amSum=sum(AM,na.rm = T),pmSum=sum(PM,na.rm = T)),]
yrMtm[, list(fullSum=sum(Full,na.rm = T),amSum=sum(AM,na.rm = T),pmSum=sum(PM,na.rm = T)),keyby=list(w)]

#test<-tradeDates[D>=ymd("2017-7-30"),c(getMTMForAll(D),getOpenPnlForPtf(D)),keyby=list(D)]


#save
write.table(yrMtm, paste0(getTradingFolder(),"ampmmtm.txt"),quote = FALSE,sep = "\t",row.names = F)

# yrMtmTest <- fread(paste0(getTradingFolder(),"ampmmtm.txt"))
# yrMtm[, list(full=sum(Full,na.rm = T),am=sum(AM,na.rm = T),pm=sum(PM,na.rm = T))]
# yrMtm[, list(fullMean=mean(Full,na.rm = T),amMean=mean(AM,na.rm = T),pmMean=mean(PM,na.rm = T))]



#open pnl
openPnl <- tr[D>ymd("2017-4-30"),getOpenPnlForPtf(D), keyby=list(D)]
openPnl[,w:=wday(D)-1]
openPnl[, sum(openpnl), keyby=list(w)]



###am pm
# trades analysis am pm analysis
#get max min info

maxMin<-tr[D>ymd("2017-4-30"),getMinuteMtmForAll(D),keyby=list(D)]

maxMin[, w:=wday(D)-1]
maxMin[, dayMaxT1:= convertTimeToDecimal(dayMaxT)]
maxMin[, dayMinT1:= convertTimeToDecimal(dayMinT)]
maxMin[, amMaxT1:= convertTimeToDecimal(amMaxT)]
maxMin[, amMinT1:= convertTimeToDecimal(amMinT)]
maxMin[, pmMaxT1:= convertTimeToDecimal(pmMaxT)]
maxMin[, pmMinT1:= convertTimeToDecimal(pmMinT)]

#get max/min time mean
maxMin[, list(daymax=mean(dayMaxT1), daymin=mean(dayMinT1),
              ammax = mean(amMaxT1), ammin = mean(amMinT1), pmmax=mean(pmMaxT1),pmmin=mean(pmMinT1)), keyby=list(w)]

#range
maxMin[, amRange:= amMax - amMin]
maxMin[, pmRange:= pmMax - pmMin]

maxMin[, mean(amRange), keyby=list(w)]
maxMin[, mean(pmRange), keyby=list(w)]

maxMin[, prevCP:= shift(percClose, 1)]


maxMin[, mean(percClose), keyby=list(w)]
maxMin[, mean(percClose), keyby=list(w,cut(prevCP,breaks = quantile(prevCP,na.rm = T),include.lowest = T ))]
maxMin[, mean(percClose), keyby=list(w,prevCP>0.5)]

#merge
fullMtmMaxMin<-merge(maxMin, yrMtm, by = "D")

#look at morning sharpe (return/sd(range))
fullMtmMaxMin[ , list(AM=mean(AM), AMRange= mean(amRange), rangeSD= sd(amRange), amSharpe = mean(AM)/sd(amRange)),, keyby=list(w.x)]
fullMtmMaxMin[ , list(PM=mean(PM), PMRange= mean(pmRange), rangeSD= sd(pmRange), pmSharpe = mean(PM)/sd(pmRange)),, keyby=list(w.x)]




# computeOpenDelta <- function(dat){
#   d<-tr[D< (dat), list(FullTicker,sum(Volume)),
#         keyby=list(FullTicker)][V2!=0,][,list(FullTicker, V2,getClosingPriceBeforeD(dat,FullTicker)),keyby=list(FullTicker)]
#   d[, sum(V2*V3)]
# }

