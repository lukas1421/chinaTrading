# trend pnl prediction

symb <- "sh000001"

d<- fread(paste0(getMinuteDataFolder(),getOneTicker(symb),".txt"),
          skip = 1,fill = T,select = c(1,2,3,4,5,6),key = "D",col.names = c("D","T","O","H","L","C"))
d <- d[!.N,]

#for shcomp only
fillData000001()
d <- generateIndexMin()
d[, D:=ymd(D)]
#d[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
#(d[,list(D,T,O,H,L,C)])

d[, .SD[T<1131][.N][,C] , keyby=list(D)]

d1 <- d[, .(open=.SD[1][,O],noon=.SD[T<1131][.N][,C],close=.SD[.N][,C]) , keyby=list(D)]

d1[, .(am=noon/shift(close,1)-1, pm=close/noon-1, cc=close/shift(close,1)-1), keyby=list(D)]
d1[, am:= noon /shift(close,1)-1]
d1[is.na(am), am:= 0]
d1[, pm:= close/noon-1]
d1[, cc:= close/shift(close,1)-1]
d1[is.na(cc),cc:=0 ]
d1[, w:= wday(D)-1]
d1[, mon:= getMonOfWeek(D)]
d1[, cumsumTrend:= cumsum(am) , keyby=list(mon)]
d1[, obsEachWeek:= .N  , keyby=list(mon)]
d2 <- d1[obsEachWeek==5, ]
d2<-d2[, .(cumtrendWed=.SD[3][,cumsumTrend], thursAM=.SD[4][,am], friAM=.SD[5][,am]), keyby=list(mon)]
d2[, qplot(cumtrendWed, thursAM)]
d2[, qplot(cumtrendWed, friAM)]

amMat<-dcast.data.table(d1, mon~w, value.var = "am")
pmMat<-dcast.data.table(d1, mon~w, value.var = "pm")
ccMat<-dcast.data.table(d1, mon~w, value.var = "cc")

amMat[,complete:=complete.cases(.SD), keyby=list(mon)]
pmMat[,complete:=complete.cases(.SD), keyby=list(mon)]
ccMat[,complete:=complete.cases(.SD), keyby=list(mon)]

cor(amMat[complete==TRUE, .(`1`,`2`,`3`,`4`,`5`) ])
cor(pmMat[complete==TRUE, .(`1`,`2`,`3`,`4`,`5`) ])
cor(ccMat[complete==TRUE, .(`1`,`2`,`3`,`4`,`5`) ])

d1[, pacf(am)]
d1[, pacf(pm)]
d1[, pacf(cc)]
