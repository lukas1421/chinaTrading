
#' get trading history from file
#' @export
getTradingHistory <- function() {
  tr <- fread(paste0(getTradingFolder(),"trades.csv"))
  tr[, D:=ymd(Date)]
  tr[, w:= wday(D)-1]
}

#' get trading dates
#' @export
getTradingDates <- function() {
  tradeDates <- fread(paste0(getTradingFolder(),"tradeDates.csv"))
  tradeDates[, D:=ymd(Date)]
  tradeDates[, w:= wday(D)-1]
}

#' get minute mtm
#' @export
#' @param dat date
#' @param symb stock
#' @param pos opening pos for that stock
getMinuteMtm <- function(dat, symb,pos) {
  symbS <- deparse(substitute(symb))
  d <- getMinuteDataPure(dat,symb)
  prev <- getClosingPriceBeforeD(dat,symb)
  d[, mtm:= (C-prev)*pos]
  d<-d[, list(T,mtm)]
  names(d) <- c("T",eval(symb))
  d
}

#' get minute delta
#' @export
#' @param dat date
#' @param symb stock
#' @param pos open position
getMinuteDelta <- function(dat, symb,pos) {
  d <- getMinuteDataPure(dat,symb)
  d[, mtm:= C*pos]
  d<-d[, list(T,mtm)]
  names(d) <- c("T",eval(symb))
  d
}


#' get minute mtm for all
#' @export
#' @param dat date
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
  #mtmForAll[, qplot(T,mtm, geom="line")]
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
  amClose <- mtmForAll[T<=1130, ][.N,mtm]

  percClose <- mtmForAll[, (dayClose - dayMin)/( dayMax - dayMin)]
  pmChgPerc <- (dayClose - amClose)/( dayMax - dayMin)

  return(list(dayMax=dayMax, dayMin=dayMin, amMax=amMax,amMin = amMin, pmMax=pmMax, pmMin = pmMin,
              dayMaxT = dayMaxT, dayMinT=dayMinT, amMaxT = amMaxT, amMinT= amMinT, pmMaxT=pmMaxT,pmMinT=pmMinT,
              dayOpen = dayOpen, dayClose= dayClose, percClose = percClose, pmChgPerc = pmChgPerc))
}

#' get the minute data mtm
#' @export
#' @param dat date
getMTMDelta <- function(dat) {
  openPos <- getOpenPos(dat)
  res <- data.table()
  # for(t in openPos$ticker) {
  #   open <- openPos[ticker==t,open]
  #   m <- getMinuteDelta(dat, t, open)
  #   print(m)
  #   if(nrow(res)==0){
  #     res <- m
  #   } else {
  #     res <- merge(res,m, by="T")
  #   }
  # }

  sapply(openPos$ticker, function(t) {
    print(paste0(" ticker ", t))
    open <- openPos[ticker==t,open]
    print(paste0(" open ", open))
    m <- getMinuteDelta(dat, t, open)
    print(m)
    if(nrow(res)==0){
      res <<- m
    } else {
      res <<- merge(res,m, by="T")
    }
  })
  print(res)

  res[, mtm:=rowSums(.SD), keyby=list(T)]
  res[, chg:= mtm/shift(mtm,1)-1,]
  res[1, chg:= 0.0]
  res[, mean:= (cumsum(chg)/.I)]
  res[, sd:= sqrt((cumsum(chg^2)/.I-(cumsum(chg)/.I)^2)*.I/(.I-1))]
  res[, sharpe:= mean/sd*sqrt(240) ]
  print(res)

  #res
}


#' get mtm for all
#' @export
#' @param dat date
getMTMForAll <-function(dat) {
  print(dat)
  tr <- getTradingHistory()
  d<-tr[D<(dat), list(FullTicker,sum(Volume)),
        keyby=list(FullTicker)][V2!=0,][,list(ticker=FullTicker,open=V2,prev=getClosingPriceBeforeD(dat,FullTicker)),
                                        keyby=list(FullTicker)]
  #print(d)
  e <- d[,c(ticker,open,getAllMTM(dat,ticker,open)),keyby=list(ticker)]
  #print(e)
  e[, list(dat,Full=sum(Full),AM=sum(AM),PM=sum(PM))]
}


#' get closing price
#' @export
#' @param dat date
#' @param symb stock symbol
#'
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

#' get minute data pure
#' @export
#' @param dat date
#' @param symb stock symbol
getMinuteDataPure <- function(dat, symb) {
  tryCatch ({
    #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    ticker <- getOneTicker(symb)
    if(exists(toupper(symb))) {
      print("exists")
      stock <- get(toupper(symb))
    } else {
      print(" no exists")
      stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                     col.names = c("D","T","O","C"),select = c(1,2,3,6))
      stock <- stock [!.N,]
    }
    stock <- stock[D==dat,list(D,T,O,C)]
    stock
  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

#' get minute data
#' @export
#' @param dat date
#' @param symb stock symbol
getMinuteDataPureWithPrevClose <- function(dat, symb) {
  tryCatch ({
    #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    ticker <- getOneTicker(symb)
    if(exists(toupper(symb))) {
      print("exists")
      stock <- get(toupper(symb))
    } else {
      print(" no exists")
      stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                     col.names = c("D","T","O","C"),select = c(1,2,3,6))
      stock <- stock [!.N,]
    }

    # stockPrev  <- stock[D<dat, ][.N, ][,list(D,T,O,C)]
    # stock <- stock[D==dat,list(D,T,O,C)]
    # res<-rbindlist(list(stockPrev,stock),use.names = TRUE,fill = TRUE)
    #return(res)
    return(d[D<=dat][(.N-240):.N])
  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

#' get minute data for all date
#' @export
#' @param symb stock symbol
getMinuteDataPureForAllDate <- function(symb) {
  tryCatch ({
    #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    ticker <- getOneTicker(symb)
    stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   col.names = c("D","T","O","C"),select = c(1,2,3,6))
    stock <- stock[-.N]
    stock[,D:=ymd(D)]
    return(stock)
  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

#' get minute data for day
#' @export
#' @param dat date
#' @param symb stock
#' @param pos opening pos for that stock
getMinuteDataForDay <- function(dat,symb,pos) {
  tryCatch ({
    #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
    ticker <- getOneTicker(symb)
    stock <- data.table()
    stock <- fread(paste0(getMinuteDataFolder(),ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                   select = c(1,2,6),col.names = c("D","T","C"))
                     #c("D","T","O","H","L","C","V","A"))
    stock <- stock [!.N,]
    stock [, D:=ymd(D)]
    ytdClose <- stock[D<dat,][.N,C]
    print(ytdClose)
    #return(stock[D==dat,list(D,T,C, (C- ytdClose)*pos)])
    stock[, pnl:= (C - ytdClose)*pos]
    stock <- stock[D==dat,list(D,T,C, pnl)]
    #print(stock)
    #pnl <- stock$pnl
    #names(pnl) <- stock$T
    #print(pnl)
    return(stock[,  list(pnl), keyby=T])

  }, error = function(err) {
    print(err)
    stock <- 0.0
    return(0.0)
  })
}

getDayMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPureWithPrevClose(dat, symb)
  (d[.N,C ] - d[1,C])*pos
}

getAMMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPureWithPrevClose(dat, symb)
  (d[T<=1130, list(T,C)][.N,C] - d[1,C])*pos
}

getPMMTM <- function(dat, symb, pos) {
  d <- getMinuteDataPure(dat, symb)
  (d[.N,C] - d[T<=1130, list(T,C)][.N,C])*pos
}

getAllMTM<- function(dat, symb, pos) {
  d <- getMinuteDataPureWithPrevClose(dat, symb)
  print(d)
  if(nrow(d)>=240) {
    return(list(Full=(d[.N,C ] - d[1,C])*pos,
                Open= (d[2,O] - d[1,C])*pos,
                AM=(d[T<=1130, list(T,C)][.N,C] - d[1,C])*pos,
                PM=(d[.N,C] - d[T<=1130, list(T,C)][.N,C])*pos))
  } else {
    return(list(Full=0,Open=0,AM=0,PM=0))
  }
}


# getMTMMinuteList<- function(dat, symb, pos) {
#   d <- getMinuteDataPureWithPrevClose(dat, symb)
#   print(d)
#   prevClose <- d[1,C]
#   d[, chg:=(C-prevClose)*pos]
#   d[-1,list(D,T,chg)]
# }


#' converting time
#' @param t time to convert
convertTimeToDecimal <- function(t) {
  hr <- floor(t/100)
  hr+(t-hr*100)/60
}

