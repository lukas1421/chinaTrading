#monthly return


d <- getDataPure("sh000001")
d[month(shift(D,1,type = "lead"))!=month(D)]
d[, monthRet:= C/shift(C,1)-1]
d[!is.na(monthRet),calcSharp(monthRet), keyby=month(D)]

#quarters
d <- getDataPure("sh000001")
d[month(shift(D,1,type = "lead"))!=month(D)]
q <- d[month(D)%%3==0, ]
