#load trades into R

library(chinaTrading)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)

tr <- fread(paste0(getTradingFolder(),"trades.csv"))
tr[, D:=ymd(Date)]
tr[, w:= wday(D)-1]

#check top/worst performer
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(-V1)][1:20]
tr[, sum(`Unrealized PL`),keyby=list(Name)][order(V1)][1:20]

#open position for a given date
tr[D< ymd("2017-8-11"), list(FullTicker,sum(Volume)), keyby=list(FullTicker)][V2!=0]



dat <-ymd("2017-8-11")
d<-tr[D< dat, list(FullTicker,sum(Volume),getClosingPriceBeforeD(dat,FullTicker)), keyby=list(FullTicker)][V2!=0]


# CAUTION LONG EXECUTION #############
yrMtm<-tr[D>ymd("2017-4-30"),getMTMForAll(D),keyby=list(D)]
############################################

yrMtm[, list(fullSum=sum(Full,na.rm = T),amSum=sum(AM,na.rm = T),pmSum=sum(PM,na.rm = T)),]



##################  get open pnl


#save
write.table(yrMtm, paste0(getTradingFolder(),"ampmmtm.txt"),quote = FALSE,sep = "\t",row.names = F)
yrMtmTest <- fread(paste0(getTradingFolder(),"ampmmtm.txt"))

yrMtm[, list(sum(Full,na.rm = T),sum(AM,na.rm = T),sum(PM,na.rm = T))]
yrMtm[, list(mean(Full,na.rm = T),mean(AM,na.rm = T),mean(PM,na.rm = T))]



#get max min info
maxMin<-tr[D>ymd("2017-8-6"),getMinuteMtmForAll(D),keyby=list(D)]

##########################FUNCTIONS########################################################################################################




getMinuteMtm <- function(dat, symb,pos) {
  symbS <- deparse(substitute(symb))
  d <- getMinuteDataPure(dat,symb)
  prev <- getClosingPriceBeforeD(dat,symb)
  d[, mtm:= (C-prev)*pos]
  d<-d[, list(T,mtm)]
  names(d) <- c("T",eval(symb))
  d
  #names(res) <- d$T
}

getMinuteMtmForAll <- function(dat) {
  openPos <- getOpenPos(dat)
  res <- data.table()
  for(t in openPos$ticker) {
    open <- openPos[ticker==t,open]
    m <- getMinuteMtm(dat, t, open)
    print(m)
    if(nrow(res)==0){
      res <- m
    } else {
      res <- merge(res,m, by="T")
    }
  }
  mtmForAll <- res[, mtm:=rowSums(.SD), keyby=list(T)]

  mtmForAll[, qplot(T,mtm, geom="line")]

  print(mtmForAll)
  dayMax <- mtmForAll[, max(mtm)]
  dayMin <- mtmForAll[, min(mtm)]
  amMax <- mtmForAll[T<1130, max(mtm)]
  amMin <- mtmForAll[T<1130, min(mtm)]
  pmMax <- mtmForAll[T>1259, max(mtm)]
  pmMin <- mtmForAll[T>1259, min(mtm)]
  dayMaxT <- mtmForAll[max(mtm)==mtm][,T]
  dayMinT <- mtmForAll[min(mtm)==mtm][,T]

  amMaxT <- mtmForAll[T<1130][max(mtm)==mtm][,T]
  amMinT <- mtmForAll[T<1130][min(mtm)==mtm][,T]

  pmMaxT <- mtmForAll[T>1259][max(mtm)==mtm][,T]
  pmMinT <- mtmForAll[T>1259][min(mtm)==mtm][,T]
  dayOpen <- mtmForAll[T==931, mtm]
  dayClose <- mtmForAll[T==1500, mtm]

  return(list(dayMax=dayMax, dayMin=dayMin, amMax=amMax,amMin = amMin, pmMax=pmMax, pmMin = pmMin,
               dayMaxT = dayMaxT, dayMinT=dayMinT, amMaxT = amMaxT, amMinT= amMinT, pmMaxT=pmMaxT,pmMinT=pmMinT,
              dayOpen = dayOpen, dayClose= dayClose))
  # #print(openPos[ticker==t,open])
  #mtmForAll
}


getMTMForAll <-function(dat) {
  d<-tr[D<(dat), list(FullTicker,sum(Volume)),
     keyby=list(FullTicker)][V2!=0,][,list(ticker=FullTicker,open=V2,prev=getClosingPriceBeforeD(dat,FullTicker)),
                                     keyby=list(FullTicker)]
  e <- d[,c(ticker,open,getAllMTM(dat,ticker,open)),keyby=list(ticker)]
  print(e)
  e[, list(dat,Full=sum(Full),AM=sum(AM),PM=sum(PM))]
}

computeOpenDelta <- function(dat){
  d<-tr[D< (dat), list(FullTicker,sum(Volume)),
        keyby=list(FullTicker)][V2!=0,][,list(FullTicker, V2,getClosingPriceBeforeD(dat,FullTicker)),keyby=list(FullTicker)]
  d[, sum(V2*V3)]
}

getClosingPriceBeforeD <- function(dat,symb) {
  tryCatch ({
    ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    stock <- data.table()
    stock <- fread(paste0(getDayDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
    stock <- stock [!.N,]
    stock [, D:=ymd(D)]
    return(stock[D<dat, ][.N,C])

  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

getMinuteDataPure <- function(dat, symb) {
  tryCatch ({
    ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    #stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
    #               showProgress = TRUE,col.names = c("D","T","O","H","L","C","V","A"))

    stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   showProgress = TRUE,col.names = c("D","T","O","C"),select = c(1,2,3,6))

    stock <- stock [!.N,]

    #return(stock[D==dat,list(D,T,C, (C- ytdClose)*pos)])
    #stockPrev  <- stock[D<dat, ][.N, ][,list(D,T,O,C)]
    stock <- stock[D==dat,list(D,T,O,C)]
    #res<-rbindlist(list(stockPrev,stock),use.names = TRUE,fill = TRUE)
    stock
  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

getMinuteDataForDay <- function(dat,symb,pos) {
  tryCatch ({
    ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    stock <- data.table()
    stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   showProgress = TRUE,col.names = c("D","T","O","H","L","C","V","A"))
    stock <- stock [!.N,]
    stock [, D:=ymd(D)]
    ytdClose <- stock[D<dat,][.N,C]
    print(ytdClose)
    #return(stock[D==dat,list(D,T,C, (C- ytdClose)*pos)])
    stock[, pnl:= (C - ytdClose)*pos]
    stock <- stock[D==dat,list(D,T,C, pnl)]
    pnl <- stock$pnl
    names(pnl) <- stock$T
    print(pnl)
    return(pnl)

  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

getDayMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPure(dat, symb)
  (d[.N,C ] - d[1,C])*pos
}

getAMMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPure(dat, symb)
  (d[T<=1130, list(T,C)][.N,C] - d[1,C])*pos
}

getPMMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPure(dat, symb)
  (d[.N,C] - d[T<=1130, list(T,C)][.N,C])*pos
}

getAllMTM<- function(dat, symb, pos) {
  d <- getMinuteDataPure(dat, symb)
  print(d)
  if(nrow(d)==241) {
    return(list(Full=(d[.N,C ] - d[1,C])*pos,
                AM=(d[T<=1130, list(T,C)][.N,C] - d[1,C])*pos,
                PM=(d[.N,C] - d[T<=1130, list(T,C)][.N,C])*pos))
  } else {
    return(list(Full=0,AM=0,PM=0))
  }
}
