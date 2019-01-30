#
# #All r update
# require(data.table)
# require(ggplot2)
# require(lubridate)
# require(stringr)
# require(reshape2)
# require(PerformanceAnalytics)
# require(quantmod)

if(Sys.getenv("USERNAME")=="LUke") {
  mainDir <- "J:\\Data\\mainBoardR\\"
  dayDataFolder <- "J:\\TDX\\T0002\\export\\"
} else if(Sys.getenv("USERNAME")=="Luke Shi") {
  mainDir <- "H:\\Data\\mainBoardR\\"
  dayDataFolder <-  "G:\\export\\"
}

#' generating index
fillData000001()

indexDay <- generateIndexDay()
indexDay[, yr:= year(D)]
indexDay[,yrOpen:=.SD[1L][,O],keyby=list(yr)]
indexDay[,yrOpenDay:=.SD[1L][,D] ,keyby=list(yr)]
indexDay[, mo:= paste0(year(D),ifelse(month(D)<10,"0",""),month(D))]
indexDay[, moOpen:=.SD[1L][,O] ,keyby=list(mo)]
indexDay[, moOpenDay:=.SD[1L][,D] ,keyby=list(mo)]

indexDay[, prevC:=shift(C,1)]
indexDay[, ytdDev:= (prevC/yrOpen-1)]
indexDay[, mtdDev:= (prevC/moOpen-1)]
indexDay <- indexDay[D>ymd("1998-12-31")]

indexM <- generateIndexMin()
resMerged <- processShcomp(indexDay,indexM)

#graphShcomp(indexDay)
#graphShcompD(indexM)

res <- data.table()
tmp <- data.table()

res1<-melt.data.table(indexM, id.vars = c("D","T"))
res2 <- dcast.data.table(res1, D ~ variable+T, sep = "", fun.aggregate = mean)

for (v in c("O","H","L","C")) {
  res2[is.na(get(paste0(v,1130))), eval(paste0(v,1130)):=get(paste0(v,1129)),]
  res2[is.na(get(paste0(v,1129))), eval(paste0(v,1129)):=get(paste0(v,1128)),]
  res2[is.na(get(paste0(v,1301))), eval(paste0(v,1301)):=get(paste0(v,1302)),]
  res2[is.na(get(paste0(v,1300))), eval(paste0(v,1300)):=get(paste0(v,1301)),]
  res2[is.na(get(paste0(v,1500))), eval(paste0(v,1500)):=get(paste0(v,1459)),]
}


#tradeTime <- c(931:959,1000:1059,1100:1130,1300:1359,1400:1459,1500)
first20 <- c(931:950)
amTime <- c(931:959,1000:1059,1100:1130)
pmTime <- c(1300:1359,1400:1459,1500)
tradeTime <- c(amTime, pmTime)

#max min
res2[, dayMax:=max(unlist(mget(paste0("H",tradeTime)))), keyby=list(D)]
res2[, dayMaxT1:=as.numeric(tradeTime[which.max(unlist(mget(paste0("H",tradeTime))))]), keyby=list(D)]
res2[, dayMin:=min(unlist(mget(paste0("L",tradeTime)))), keyby=list(D)]
res2[, dayMinT1:=as.numeric(tradeTime[which.min(unlist(mget(paste0("L",tradeTime))))]), keyby=list(D)]

res2[, first20AboveO:=sum(mget(paste0("C",first20))>O931)/20,keyby=list(D)]

res2[, amMax:=max(unlist(mget(paste0("H",amTime)))), keyby=list(D)]
res2[, amMaxT1:= as.numeric(amTime[which.max(unlist(mget(paste0("H",amTime))))]), keyby=list(D)]
res2[, amMin:=min(unlist(mget(paste0("L",amTime)))), keyby=list(D)]
res2[, amMinT1:=as.numeric(amTime[which.min(unlist(mget(paste0("L",amTime))))]), keyby=list(D)]

res2[, pmMax:=max(unlist(mget(paste0("H",pmTime)))), keyby=list(D)]
res2[, pmMaxT1:=as.numeric(pmTime[which.max(unlist(mget(paste0("H",pmTime))))]), keyby=list(D)]
res2[, pmMin:=min(unlist(mget(paste0("L",pmTime)))), keyby=list(D)]
res2[, pmMinT1 := as.numeric(pmTime[which.min(unlist(mget(paste0("L",pmTime))))]), keyby=list(D)]


# MERGE ########################################################################################
resMerged <- merge(indexDay,res2,by = "D" )
resMerged[, weekday:= wday(D)-1]
resMerged[, range:= log(dayMax/dayMin)]
resMerged[, first10:= log(C940/O931)]
resMerged[, first1:=log(C931/O931)]
resMerged <- merge(indexDay,res2,by = c("D"))
resMerged[, retCL:= log(C/L)]
resMerged[, retLO:= log(L/O)]
resMerged[, retHO:= log(H/O)]
resMerged[, retCH:= log(C/H)]
resMerged[, retCO:= log(C/O)]
resMerged[, rangeDay:= log(H/L)]
resMerged[, openPercentile:=(O - dayMin)/(dayMax - dayMin)]
resMerged[, openPercentileY:= shift(openPercentile,1)]
resMerged[, openPercentileYCat:=cut(openPercentileY, quantile(openPercentileY,na.rm = T),include.lowest = T)]
resMerged[, retAM:= shift(C1130/O)]
resMerged[, retPM:= shift(C/O1300)]
resMerged[, retOPC:= log(O/shift(C,1))]
resMerged[is.na(retOPC), retOPC:=0]
resMerged[, retAMHO := log(amMax/O)]
resMerged[, retPMCH := log(C/pmMax)]
resMerged[, percentile:= (C-L)/(H-L)]
resMerged[, percentileY:=shift(percentile,1)]
resMerged[, dayMinY:= shift(dayMin,1)]
resMerged[, retPMClose1315:= log(C/O1315)]
resMerged[, retAMCO:= log(C1130/O)]
resMerged[, retAMCOY:= shift(retAMCO,1)]
resMerged[, retPMCO:=log(C/O1300) ]
resMerged[, retPMCOY:= shift(retPMCO,1)]
resMerged[is.na(retPMCOY), retPMCOY:=0]
resMerged[, retPMCOYCat:= cut(retPMCOY, unique(quantile(retPMCOY,na.rm = T)),include.lowest = T)]
resMerged[, retPMCL:= log(C/pmMin)]
resMerged[, retPMHO:= log(pmMax/O1300)]
resMerged[, pmRange:= log(pmMax/pmMin)]
resMerged[, pmclOverPmRange:= retPMCL/pmRange]
resMerged[, pmclOverPmRangeY:=shift(pmclOverPmRange,1)]
resMerged[, pmclOverPmRangeCat:= cut(pmclOverPmRange,quantile(pmclOverPmRange,na.rm = T),include.lowest = T)]
resMerged[, pmclOverPmRangeYCat:= cut(pmclOverPmRangeY,quantile(pmclOverPmRangeY,na.rm = T),include.lowest = T)]
resMerged[, retPMCH:= log(C/pmMax)]
resMerged[, pmHOCHRatio:= (retPMHO-retPMCH)/pmRange]
resMerged[, pmHOCHRatioY:= shift(pmHOCHRatio,1)]
resMerged[, pmHOCHRatioYCat:= cut(pmHOCHRatioY, quantile(pmHOCHRatioY,na.rm = T),include.lowest = T) ]
resMerged[, retPMLO:= log(pmMin/O1300)]
resMerged[, pmLOCLRatio:= (retPMLO-retPMCL)/pmRange]
resMerged[, pmLOCLRatioY:= shift(pmLOCLRatio,1)]
resMerged[, pmLOCLRatioYCat:=cut(pmLOCLRatioY,breaks = quantile(pmLOCLRatioY,na.rm = T),include.lowest = T)]
resMerged[, amCP:= (C1130 - amMin)/(amMax - amMin )]
resMerged[, amCPY:= shift(amCP,1)]
resMerged[,amCPYCat:=cut(amCPY,breaks = quantile(amCPY,na.rm = T),include.lowest = T)]
resMerged[, mean(retPMCO), keyby=list(amCPYCat)]
resMerged[, HOCHRatio:= (retHO - retCH)/rangeDay ]
resMerged[, AMPMRatio:= (retAM-retPM)/rangeDay]
resMerged[, HOCHRatioY:=  shift(HOCHRatio,1)]
resMerged[, AMPMRatioY:= shift(AMPMRatio,1)]
resMerged[, HOCHRatioYCat:= cut(HOCHRatioY,breaks = quantile(HOCHRatioY,na.rm = T),include.lowest = T)]
resMerged[, AMPMRatioYCat:= cut(AMPMRatioY,quantile(AMPMRatioY,na.rm = T),include.lowest = T)]
resMerged[is.na(percentileY), percentileY:= 0.5]
resMerged[, percentileCat:=cut(percentile, breaks = quantile(percentile),include.lowest = T)]
resMerged[, percentileYCat:=cut(percentileY, breaks = quantile(percentileY,na.rm = T),include.lowest = T)]
resMerged[, weekday:= wday(D)-1]
print(resMerged)
resMerged

###
#indexDay <- generateIndexDay()
#indexM <- generateIndexMin()
#index942 <- indexM[T <= 942, .(max(H),T[which(H==max(H))][1], min(L),T[which(L==min(L))][1]), keyby=list(D)]
#indexDay[, CC:=C/shift(C,1)-1]
#indexDay[, CO:=C/O-1]
####

n <- data.table(time,t)

for(i in c(932:959,1000:1059,1100:1130,1300:1359,1400:1459)) {print(i); a <- summary(indexM[T==i,  ][, lm(retToClose~percAbove)]); t <- a$coefficients[2,3]; n<-rbind(n, list(i,t)); print(n)}


