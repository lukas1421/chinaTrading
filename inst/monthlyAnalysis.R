#monthly analysis 

indexDay[, .(D)]
indexDay[, y:=year(D)]
indexDay[, m:= month(D)]
indexDay[, d:=day(D)]
monthly <- indexDay[m!= shift(m,1,type = "lead")]
monthly[, ret := C/shift(C,1)-1]
monthly[!is.na(ret), calcSharp(ret), keyby=list(m)]


