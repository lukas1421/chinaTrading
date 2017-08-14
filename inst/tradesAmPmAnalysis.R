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
