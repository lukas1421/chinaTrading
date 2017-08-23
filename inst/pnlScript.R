#portfolio analysis



#wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"Trade List.xlsx"),create = TRUE)

library(data.table)
library(chinaTrading)
library(ggplot2)
library(lubridate)

pnl<-fread(paste0(getTradingFolder(),"assetLine.csv"),skip = 0,stringsAsFactors = FALSE,blank.lines.skip = TRUE)
pnl[, D:= ymd(Date)]
pnl[, w:= wday(D)-1]


####################################################################################################
pnl[, qplot(D,NetPtf,geom = "line")]
pnl[, list(Bot=mean(`Delta bought`,na.rm = T),Sold=mean(`Delta Sold`,na.rm = T), Cl=mean(`Delta close`,na.rm = T)),keyby=list(w)]
# pnl[, mean(`Delta Sold`,na.rm = T),keyby=list(w)]


#weekday and delta
pnl[, mean(`Delta close`,na.rm = T),keyby=list(w)]

#Trade pnl
pnl[, list(Tr=sum(`Trade pnl`), Buy=sum(`Trade pnl buy`), Sell=sum(`Trade pnl sell`), mtm = sum(MTM), total =sum(MTM+`Trade pnl`)), keyby=list(w)]

#return on delta
pnl[, list(posDelta = mean(`Rtn on +D`,na.rm = T), negDelta = mean(`Rtn on -D`,na.rm = T),
           Bot=mean(`Delta bought`,na.rm = T),Sold=mean(`Delta Sold`,na.rm = T),
           Buy=sum(`Trade pnl buy`), Sell=sum(`Trade pnl sell`),mtm = sum(MTM)
           ),keyby=list(w)]

#check max loss days
pnl[`Actual ptf change` < -30000, list(D,weekday, `Actual ptf change`, MTM, `Trade pnl`)]


#check crash days
pnl[`Actual ptf change` < -30000, list(D,weekday, `Actual ptf change`, MTM, `Trade pnl`,`Delta close`, deltaPrev)]


#graph pnl
pnl[, qplot(D,NetPtf,geom = "line")]

#Graph ptf growth
pnl[is.na(V23), V23]
pnl[, qplot(D, cumprod(1+V23),geom = "line")]

