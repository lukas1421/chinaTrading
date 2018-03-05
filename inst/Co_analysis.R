#check CO
library(data.table)
library(ggplot2)
ticker <- "sh000001"
d <- getDataPure(ticker)
d[, CO:= C/O-1]
d[, OPC:= O/shift(C,1)-1]
d[!is.na(OPC), sum(OPC)]
d[, sum(CO)]
d[, qplot(D, cumprod(1+CO),geom = "line")]
d[!is.na(OPC),qplot(D,cumprod(1+OPC),geom="line") ]

