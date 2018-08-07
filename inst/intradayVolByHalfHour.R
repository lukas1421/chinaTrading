#compute longest run



#which 30 min is the most volatile - first 30 minutes (40% of most volatile periods occur within first 30 min)
amhalf1 <- indexM[T<1000, .(h1=(max(H)/(min(L))-1)), keyby=list(D)]
amhalf2 <- indexM[T>=1000 & T<1030, .(h2=(max(H)/(min(L))-1)), keyby=list(D)]
amhalf3 <- indexM[T>=1030 & T<1100, .(h3=(max(H)/(min(L))-1)), keyby=list(D)]
amhalf4 <- indexM[T>=1100 & T<1130, .(h4=(max(H)/(min(L))-1)), keyby=list(D)]
pmhalf1 <- indexM[T>=1300 & T<1330, .(h5=(max(H)/(min(L))-1)), keyby=list(D)]
pmhalf2 <- indexM[T>=1330 & T<1400, .(h6=(max(H)/(min(L))-1)), keyby=list(D)]
pmhalf3 <- indexM[T>=1400 & T<1430, .(h7=(max(H)/(min(L))-1)), keyby=list(D)]
pmhalf4 <- indexM[T>=1430 & T<1501, .(h8=(max(H)/(min(L))-1)), keyby=list(D)]

halfBig <- merge(amhalf1,amhalf2, by = "D")
halfBig <- merge(halfBig, amhalf3, by="D")
halfBig <- merge(halfBig, amhalf4, by="D")
halfBig <- merge(halfBig, pmhalf1, by="D")
halfBig <- merge(halfBig, pmhalf2, by="D")
halfBig <- merge(halfBig, pmhalf3, by="D")
halfBig <- merge(halfBig, pmhalf4, by="D")


halfBig[, maxHalf:=which.max(.SD) ,keyby=list(D)]
halfBig[, weekday:= wday(D)-1]
halfBig[, nrow(.SD), keyby=list(weekday,maxHalf)]


#which hour is most volatile, first and last hour more volatilj
h1 <- indexM[T<1030, .(firstHour=(max(H)/(min(L))-1)), keyby=list(D)]
h2 <- indexM[T>=1030 & T<=1130, .(secHour=(max(H)/(min(L))-1)), keyby=list(D)]
h3 <- indexM[T>=1300 & T<1400, .(thirdHour=(max(H)/(min(L))-1)), keyby=list(D)]
h4 <- indexM[T>=1400 & T<1501, .(fourthHour=(max(H)/(min(L))-1)), keyby=list(D)]

hBig <- merge(h1,h2, by = "D")
hBig <- merge(hBig, h3, by="D")
hBig <- merge(hBig, h4, by="D")
hBig <- merge(hBig, h3, by="D")
hBig[,which.max(.SD) ,keyby=list(D)]
hBig[, maxHr:=which.max(.SD) ,keyby=list(D)]
hBig[, nrow(.SD), keyby=list(maxHr)]


#check pnl from MA stratey (parameter: MA#, trade period: whole day/which hour/which half hour)
indexM[, ma5:=rollmean(C,5,fill = NA,align = "right"), keyby=list(D)]
indexM[, ma10:=rollmean(C,10,fill = NA,align = "right"), keyby=list(D)]
indexM[, ma5Prev:= shift(ma5,1), keyby=list(D)]
indexM[, ma10Prev:= shift(ma10,1), keyby=list(D)]
indexM[, signal:=ifelse(is.na(ma5) | is.na(ma10),"",
                        ifelse(ma5Prev<ma10Prev & ma5>ma10, "buy", ifelse(ma5Prev>ma10Prev & ma5<ma10, "sell","")))]

indexM[, hourBreak:=ifelse(T>=930 & T<1030,"H1",
                           ifelse(T>=1030 & T<1200,"H2",ifelse(T>=1300 & T<1400,"H3",ifelse(T>=1400 & T<1501,"H4","" )))) ]

indexM[, halfDayBreak:=ifelse(T>=930 & T<1200,"AM","PM") ]


