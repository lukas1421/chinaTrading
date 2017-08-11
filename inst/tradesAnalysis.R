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



##########################FUNCTIONS
getOpenPos <- function(dat) {
  tr[D< (dat), list(FullTicker,sum(Volume)),
     keyby=list(FullTicker)][V2!=0,][,list(FullTicker,V2,getClosingPriceBeforeD(dat,FullTicker)),
                                     keyby=list(FullTicker)]
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

