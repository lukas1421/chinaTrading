
library(chinaTrading)
library(data.table)
library(ggplot2)

#analyse pmb detailed data

pmb <- fread(paste0(getTradingFolder(),"pmbOutput.txt"),header = TRUE,skip = 0,fill = F,
                  showProgress = TRUE,col.names = c("T","P"))


pmb[, rowID:= .I]
pmb[, newT:=strptime(T, "%H:%M:%OS")]
op <- options(digits.secs=3)
qplot(newT, P, data = pmb,geom = "line")

pmb[, newMax:= cummax(P)]
pmb[, newMin:=cummin(P)]
pmb[, ]


