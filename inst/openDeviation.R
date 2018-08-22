#test open deviation

library(chinaTrading)
indexM <- generateIndexMin()


indexM[, o931:= O[T==931], keyby=D]
indexM[, c1500:= C[T==1500], keyby=D]
indexM[, list(crosses=sum(containsO931), dayret=dayRet[T==931]), keyby=list(D)]
#sd

indexDay[, ret:= C/shift(C,1)-1]
indexDay[, sd:= rollapply(ret,20,sd, align="right", fill=NA)]
indexDay[, sdAnn:=sd*sqrt(252)]

new1[, mean(dayret,na.rm = T)*10000, keyby=list(cut(crosses,breaks=quantile(crosses),include.lowest = T), cut(sdAnn, breaks=quantile(sdAnn),include.lowest = T))]
