
maxMinPerc <- function(i,dataM,dataDay) {
  tLimit <- i
  f15 <- dataM[T <= tLimit, .(max(H),T[which(H==max(H))][1], min(L),T[which(L==min(L))][1]), keyby=list(D)]
  print(f15)
  #dataDay <- getDataPure("sh000016")
  #dataDay[, p:= (C-L)/(H-L)]
  #dataDay[, CH:= (C-H)/(H-L)]
  m<-merge(f15,dataDay,by.x = "D", by.y = "D")
  m[, maxAfterMin:= ifelse(V2>V4,1,0)]
  m[, perc:=(C-L)/(H-L)]
  res <- m[, lm(perc~maxAfterMin)]
  summary(res)

  return(list(tLimit,coef(summary(res))[,"t value"]["maxAfterMin"],coef(summary(res))[,"Pr(>|t|)"]["maxAfterMin"]))
}

resultTable <- data.table(c(931:959,1000), 0)

resultTable <- resultTable[, maxMinPerc(V1,indexM, indexDay), keyby=list(V1)]

for(i in c(931:959,1000)) {
  print(i)
  print(maxMinPerc(i))
}


dataMin <- getDataPureD("sh000001")
dataDay <- getDataPure("sh000001")
indexM[T <= 942, .(max(H),T[which(H==max(H))][1], min(L),T[which(L==min(L))][1]), keyby=list(D)]


