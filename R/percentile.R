#percentile


#' get percentile of all stocks
#' @export
getPercentileAll <- function() {
  d<- fread(paste0(getTradingFolder(),"test.txt",header = FALSE))
  d<- d[,list("Ch"=V2,weekReturn=getPercentile(V1)),keyby=list(V1)]
  #names(d) <- c("Ticker", "Perc")
  return(d[, ])
}

#' get wtd percentile
#' @export
#' @param symb stock symbol
getWtdPercentile <- function(symb) {
  weekBeginningDate <- getMonOfWeek(Sys.Date())
  d <- getDataPure(symb)
  d <- d[D>= weekBeginningDate]
  if(nrow(d)>0) {
    # d[, maxWeek:= max(H)]
    # d[, minWeek:= min(L)]
    # d[, cummax:= cummax(H)]
    # d[, cummin:= cummin(L)]
    # d[, perc:= (C-cummin)/(cummax-cummin)]
    # return(list(perc=d[.N, perc]))

    return(list(perc=getPercentileCpp(d)))
  } else {
    return(list(0))
  }
}


#get
getWtdPercentileAll<- function(){
  d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<- d[,list(getWtdPercentile(V1)),keyby=list(V1)]
  names(d) <- c("Ticker", "Perc")
  return(d)
}

#' get wtd percentile
#' @param symb stock symb
#' @export
getWtdMaxMin <- function(symb) {
  m<-getMonOfWeek(Sys.Date())
  #print(m)
  d<- getDataPure(symb)

  if(nrow(d)==0) {
    return()
  }

  d<- d[D>=m]
  if(nrow(d) > 0) {
    d[, cummax := cummax(H)]
    d[, cummin := cummin(L)]
    print(d[D>=m])
    as.list(d[.N, .(cummax,cummin,C,perc=(C-cummin)/(cummax-cummin), potential=(cummax+cummin)/2/C-1 )])
  } else {
    return()
  }
}

#' get week to date max and min for all stocks
#' @export
getWtdMaxMinAll <- function() {
  print("getting wtd max min all")
  #print(getTradingFolder())
  res<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  res<- res[,(getWtdMaxMin(V1)),keyby=list(V1)]
  write.table(res, paste0(getTradingFolder(),"wtdMaxMin.txt"),quote = FALSE,sep = "\t", row.names = FALSE)
  res
}



#' ytd low date
#' @export
#' @param symb stock symbol
getYtdLowDate <- function(symb) {
  data <- getDataPure(symb)
  return(list(lowDate=data[D>ymd("20170101")][min(L)==L][1][,D]))
}

#' getYtdpercentile
#' @export
#' @param symb the stock in question
getYtdPercentile <- function(symb) {
  d <- getDataPure(symb)
  d <- d[D>ymd("20161231")]
  # d[, ytdMax:=cummax(H)]
  # d[, ytdMin:= cummin(L)]
  # print(d[.N, (C-ytdMin)/(ytdMax-ytdMin)])
  #return(list(perc=d[,(C[.N]-min(L))/(max(H)-min(L))]))
  return(list(perc=getPercentileCpp(d)))
}

getYtdPercentileAll <- function() {
  res<- fread(paste0(tradingFolder,"test.txt"),header = FALSE)
  res<- res[,(getYtdPercentile(V1)),keyby=list(V1)]
  res
}


getPercentile <- function(symb) {
  d <- getData(symb)
  return(d[.N, list(percentileAll)][1]$percentileAll)
}
