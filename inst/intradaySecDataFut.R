options(digits.secs = 3)
library(data.table)
library(ggplot2)
library(chinaTrading)

tmp <- fread(paste0(getTradingFolder(),"pmbSGXA50.txt"))
names(tmp) <- c("T", "P")
tmp[, t:= strptime(T,format = "%H:%M:%OS")]
tmp[, minformat:= format(t, "%H:%M")]
tmp[, secformat:= format(t, "%H:%M:%S")]
tmp[, chg:= P-shift(P,1)]
tmp[is.na(chg),chg:=0 ]

tmp[,.N ,keyby=list(minformat)]
tmp[,.N ,keyby=list(secformat)]

tmp[,.N ,keyby=list(secformat)]

tmp[, list(.N, sum(chg)) ,keyby=list(minformat)]
tmp[, list(.N, sum(chg)) ,keyby=list(secformat)]
#graph
tmp[, qplot(t, P, geom="line")]
tmp[t < as.POSIXct("2018-09-11 13:45:00") & t > as.POSIXct("2018-09-11 13:00:00")
      , qplot(t, P, geom="line")]
tmp[t < as.POSIXct("2018-09-11 09:33:00") & t > as.POSIXct("2018-09-11 9:29:00"), .(t)]
