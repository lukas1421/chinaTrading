#' analyze SS
#' @export
#' @import rvest
#' @import data.table
#' @importFrom lubridate ymd
analyzeSS <- function(symb) {

  dt<- getData(symb)

  dt[, LO:=L/O-1]
  dt[, LOY:=shift(LO,1)]
  dt[is.na(LOY),LOY:=0]
  dt[, CO:=C/O-1]
  dt[, CC:=C/shift(C,1)-1]
  dt[, CL:=C/L-1]
  dt[, CLY:=shift(CL,1)]
  dt[is.na(CLY),CLY:=0]
  dt[, HO:=H/O-1]
  dt[, HOY:=shift(HO,1)]
  dt[, CH:=C/H-1]
  dt[, sd5:=rollapplyr(CC,5,sd,fill=NA)]
  dt[, sd10:=rollapplyr(CC,10,sd,fill=NA)]
  dt[, sd20:=rollapplyr(CC,20,sd,fill=NA)]
  dt[, max3m:= rollapplyr(H, 60, max, fill=NA)]
  dt[, min3m:= rollapplyr(L, 60, min, fill=NA)]
  dt[!is.na(max3m), perc3m := (C-min3m)/(max3m-min3m)]

  dt[is.na(HOY),HOY:=0]
  dt[, HL:=H/L-1]
  dt[, OPC:=O/shift(C,1)-1]
  dt[, percentile:=(C-L)/(H-L)]
  dt[, percentileY:=shift(percentile,1)]

  dt [ , weekday := wday(D)-1]
  print(dt[,list(CC=mean(CC,na.rm = T),CO=mean(CO,na.rm = T),CL=mean(CL,na.rm = T), CH = mean(CH,na.rm = T) ), keyby=list(weekday)])
  print( dt[, calcSharp(CO),keyby=list(weekday)])

  data <<- dt
  #return(dt)
  #print(dt[, list(CO=mean(CO),LO=mean(LO), HL=mean(HL), CL=mean(CL), HO=mean(HO),CH=mean(CH), OPC=mean(OPC,na.rm = T),
  #               corHoHoy=cor(HO,HOY), pacfHO=pacf(HO)[[1]][1],pacfCH=pacf(CH)[[1]][1])], sd5=sd5, sd10=sd10, sd20=sd20)

  invisible()

}



analyzeMin <- function(data) {
  colnames(data) <- c("d","t","o","h","l","c","v","vc")
  #d <- d[max(t)==1500,keyby=list(d)]
  d1 <- data[ d!=as.list(data[,list(maxt=max(t)) ,keyby=list(d)][maxt<1500, list(d)]),]
  d1<-d1[,list(dayH=max(h), dayL=min(l), dayO=o[t==931], dayC=c[t==1500], amMax=max(h[t<1200]),
               amMin=min(l[t<1200]),pmMax=max(h[t>1200]), pmMin=min(l[t>1200]), amMaxT= t[which.max(h[t<1200])],
               amMinT=t[which.min(l[t<1200])], pmMaxT=t[t>1200][which.max(h[t>1200])],
               pmMinT=t[t>1200][which.min(l[t>1200])], amClose=c[t==max(t[t<1200])]),  keyby=list(d)]
  d1[, amho:= amMax/dayO-1]
  d1[, pmcl:= dayC/pmMin-1]

  #d1[d>ymd("2017/3/1"), max(t), keyby=list(d)]
  return(d1)
}


get2Returnseries <- function(symb1, symb2) {
  dt1<-getDataPure(symb1)
  dt1[, eval(symb1):= log(C/shift(C,1)) ]
  dt2<-getDataPure(symb2)
  dt2[, eval(symb2):= log(C/shift(C,1)) ]
  dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(symb2))],by = "D")
  names(dt3) <- c("D",(eval(symb1)),(eval(symb2)))
  print("d3")
  print(dt3)
  dt3<-dt3[!is.na(get(symb1)),]
  print("d3 after")
  print(dt3)
  print(" #######  Beta #########")
  print(dt3[,cov(get(symb1), get(symb2),use="complete.obs")/var(get(symb2),na.rm = T)])
  return(dt3)
}

calcBeta <- function(symb1) {
  dt1 <- getDataPure(symb1)
  dt1 <- dt1[L>0]
  dt1[, eval(symb1):= log(C/shift(C,1))]
  res <- list()
  for(index in benchList) {
    #print(index)
    dt2 <- get(index)
    dt2[, eval(index):= log(C/shift(C,1)) ]
    dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(index))],by = "D")
    names(dt3) <- c("D",(eval(symb1)),(eval(index)))
    dt3<-dt3[!is.na(get(symb1)),]
    output <- dt3[,cov(get(symb1), get(index),use="complete.obs")/var(get(index),na.rm = T)]
    #print()
    #print(CAPM.beta(as.xts.data.table(dt3[, list(D,get(symb1))]),as.xts.data.table(dt3[, list(D,get(index))]) ))
    res[[index]] <- output
  }
  print(res)
  return(res)
}

calcAlpha <- function(symb1) {
  dt1 <- getDataPure(symb1)
  dt1 <- dt1[L>0]
  dt1[, eval(symb1):= log(C/shift(C,1))]
  print(dt1)
  res <- list()
  for(index in benchList) {
    #print(index)
    dt2 <- get(index)
    dt2[, eval(index):= log(C/shift(C,1)) ]
    dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(index))],by = "D")
    names(dt3) <- c("D",(eval(symb1)),(eval(index)))
    dt3<-dt3[!is.na(get(symb1)),]
    output <- CAPM.alpha(as.xts.data.table(dt3[, list(D,get(symb1))]),as.xts.data.table(dt3[, list(D,get(index))]))
    #dt3[,cov(get(symb1), get(index),use="complete.obs")/var(get(index),na.rm = T)]
    #print(CAPM.beta(as.xts.data.table(dt3[, list(D,get(symb1))]),as.xts.data.table(dt3[, list(D,get(index))]) ))
    res[[index]] <- output
  }
  print(res)
  return(res)
}

#' @export
calcGen <- function(f) {
  d<- fread(paste0(getDayDataFolder(),"test.txt"),header = FALSE)
  d<- d[1:10,c(V2,f(V1)),keyby=list(V1)]
  names(d)[1:2] <- c("Ticker","Ch")
  return(d)
}


getCrashReturn <- function(symb1,index, thresh) {
  dt1<-getData(symb1)
  dt1[, eval(symb1):= log(C/shift(C,1)) ]
  dt2 <- get(index)
  dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(index))],by = "D")
  names(dt3) <- c("D",(eval(symb1)),(eval(index)))
  res <- dt3[get(index) < thresh, do.call(mean,list(get(symb1),na.rm=T))]
  return(res)
}

getCrashReturnAll <- function() {
  d<- fread(paste0(getDayDataFolder(),"test.txt"),header = FALSE)
  d<- d[,list(V2,getCrashReturn(V1,"sh000001",-0.03)),keyby=list(V1)]
  names(d) <- c("Ticker","Ch","Res")
  d<-d[order(-as.numeric(Res))]
  return(d)
}



####################################### GRAPH #########################
graph <- function(symb,dateStr) {

  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  d<- fread(paste0(getDayDataFolder(),ticker,".txt"),skip = 1,fill = T,showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
  d <- d[!.N,]
  d[, D:=ymd(D)]
  g<-d[,list(Open=O,High=H,Low=L,Close=C),]
  g<-xts(g,order.by = d$D)
  candleChart(g[paste0(dateStr,'/20190101')], theme="white",type="candles")
}

graphD <- function(symb,dateStr) {
  #require(xts)
  #require(quantmod)
  #dataFolder <- "J:\\TDX\\T0002\\export_1m\\"

  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  d<- fread(paste0(getDayDataFolder(),ticker,".txt"),skip = 1,fill = T,showProgress = TRUE,col.names = c("D","T","O","H","L","C","V","A"))
  d <- d[!.N,]
  d[, D:=ymd(D)]
  d[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]

  g<-d[,list(Open=O,High=H,Low=L,Close=C),]
  g<-xts(g,order.by = d$DT)

  print(paste0(dateStr,'/20190101'))
  print(dateStr)
  temp <- paste0(dateStr,"/20190101")
  print(temp)
  candleChart(g[temp], theme="white",type="candles")
  #return(g)
}

#' @export
computeWeekday <- function(symb) {
  d <- getData(symb)
  d[, weekday:=wday(D)-1]
  d[, CO:=log(C/O)]
  d[, CH:=log(C/H)]
  d[, CL:=log(C/L)]
  d[, HL:=log(H/L)]
  d[, HO:=log(H/O)]
  d[, LO:=log(L/O)]
  d[, CC:= log(C/shift(C,1))]
  d[, percentile:= (C-L)/(H-L)]
  d[, percentileY:= shift(percentile,1)]
  d[, percentileYCat:=cut(percentileY, breaks = quantile(percentileY,na.rm = T),include.lowest = T)]
  d[, COY:=shift(CO,1)]
  d[, HOY:= shift(HO, 1)]
  d[, CHY:= shift(CH, 1)]
  d[, HLY:= shift(HL, 1)]
  d[, HOCHYOverRange:= (HOY-CHY)/HLY]
  d[, HOYCat:= cut(HOY, breaks = quantile(HOY,na.rm = T),include.lowest = T)]
  d[, CHYCat:= cut(CHY, breaks = quantile(CHY,na.rm = T),include.lowest = T)]
  d[, HOCHYOverRange:= (HOY-CHY)/HLY]


  print ( paste0(" Inception Date : ", d[1,D] ))
  print(paste(" COY serial corr ", d[!is.na(COY),cor(CO,COY), keyby=list(weekday)]))

  print ( "##################  BENCH  ############################")
  getCorrelGen(symb)

  print( "############ General Strength ###############")
  print(d[, calcSharp(CO), keyby=list(weekday)])


  print( "############ Bull Strength ##############")
  print(d[bull20Y==TRUE, calcSharp(CO), keyby=list(weekday)])

  print( "############ BEAR Strength #################")
  print(d[bull20Y==FALSE, calcSharp(CO), keyby=list(weekday)])

  print( "############ CL STRENGTH ################")
  print(d[, mean(CL), keyby=list(weekday)])


  print(paste0("##################", " percentileY ","#####################"))
  print(d[, calcSharp(CO), keyby=list(weekday,percentileYCat)])

  print(d[, calcSharp(CO), keyby=list(weekday)][,list(sr)])


  return(d)
}


getHOCHInfo <-function(symb) {
  d<-getData(symb)

  d[, HO:=log(H/O)]
  d[, range:=log(H/L)]
  d[, CH:= log(C/H)]
  d[, CO:= log(C/O)]
  d[, HOCHOverRange:= (HO-CH)/range]
  print(d)
  d[, HOCHOverRangeY:= shift(HOCHOverRange,1)]
  d[, HOCHOverRangeYCat:=cut(HOCHOverRangeY, quantile(HOCHOverRangeY,na.rm = T),include.lowest = T)]
  d[, weekday:=wday(D)-1]
  print(d[, calcSharp(CO), keyby=list(weekday, HOCHOverRangeYCat)])

}

computeWeekdayCLPure<-function(symb) {
  d <- getData(symb)
  d[, weekday:=wday(D)-1]
  d[, CL:=log(C/L)]

  output <- d[, list(CL=mean(CL)), keyby=list(weekday)][, list(CL)]
  output<- as.list(output$CL)
  names(output) <- c("1","2","3","4","5")
  return(output)
}

computeWeekdayPure <- function(symb,bull) {
  d <- getData(symb)
  d[, weekday:=wday(D)-1]
  d[, CO:=log(C/O)]
  d[, percentile:= (C-L)/(H-L)]
  d[, percentileY:= shift(percentile,1)]
  d[, percentileYCat:=cut(percentileY, breaks = quantile(percentileY,na.rm = T),include.lowest = T)]
  d[, COY:=shift(CO,1)]

  output <- d[bull20Y==bull, calcSharp(CO), keyby=list(weekday)][, list(sr)]
  output<- as.list(output$sr)
  names(output) <- c("1","2","3","4","5")
  return(output)
}

computeWeekdayFun <- function(symb, col) {
  require(pryr)

  d <- getData(symb)
  d[, weekday:=wday(D)-1]
  d[!is.na(weekday), CO:=(C/O-1)]
  #d[, percentile:= (C-L)/(H-L)]
  #d[, percentileY:= shift(percentile,1)]
  #d[, percentileYCat:=cut(percentileY, breaks = quantile(percentileY,na.rm = T),include.lowest = T)]
  #d[, COY:=shift(CO,1)]

  output <- d[!is.na(weekday) , calcSharp(CO), keyby=list(weekday)]

  var <- deparse(substitute(col))

  print(output[, list(sr)])

  print(output[, eval(parse(text = var))])

  print(output[, list(eval(parse(text = var)))])

  res <- output[, .SD[[var]]]

  names(res) <- c("1","2","3","4","5")
  return(as.list(res))
}




testSub <- function(col) {
  print(substitute(col))
  print(list(col=1))
  l <- list(col=1)
  names(l) <- c(deparse(substitute(col)))
  return(l)
}

getWeekdayFunAll <- function(...) {
  d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<-d[1:10, c(V2,computeWeekdayPureAll(V1,...)), keyby=list(V1)]
  print(d)
  return(d)
}

getWeekdayCL <- function() {
  #d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<-d[, c(V2,computeWeekdayCLPure(V1)), keyby=list(V1)]
  print(d)
  return(d)
}


#write.table(d, paste0("C:\\Users\\",Sys.getenv("USERNAME"),"\\Desktop\\Trading\\bench.txt",quote = FALSE,sep = "\t"))
#filling benchlist


getWeekReturn<- function(symb) {
  d <- getDataPure(symb)
  return(d[, log(C[.N]/C[.N-5])])
}

getLastWeekEachDayReturn <- function(symb) {
  d <- getDataPure(symb)
  d[, rtn:= (C/shift(C,1)-1)]
  #t<-d[(.N-4):.N, .(D,rtn)]
  t<-d[D>ymd("2017-5-12"), .(D,rtn)]
  if(nrow(t)==5) {
    t1 <-as.list(t$rtn)
    names(t1) <-t$D
    return(t1)
  }
  return()
}

getLastWeekEachDayReturnAll <- function() {
  d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<- d[, getLastWeekEachDayReturn(V1), keyby=list(V1)]
  return(d[, ])
}


getWeekReturnAll <- function() {
  d<- fread(paste0("C:\\Users\\",Sys.getenv("USERNAME"), "\\Desktop\\Trading\\test.txt",header = FALSE))
  #d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<- d[,list("Ch"=V2,weekRtn=getWeekReturn(V1)),keyby=list(V1)]
  return(d[, ])
}





calcFriSharpe <- function(symb) {
  d <- getDataPure(symb)
  d[, cc:= (C/shift(C,1)-1)]
  d[, w:=wday(D)-1]
  m <- d[,mean(cc,na.rm=T)]
  sd <- d[, sd(cc,na.rm=T)]
  return(list(sr=m/sd))
}

getFriSharpeAll <- function() {
  res<-fread(paste0(tradingFolder,"test.txt"),header = FALSE)
  res <- res[, calcFriSharpe(V1), keyby=list(V1)]
  res
}



