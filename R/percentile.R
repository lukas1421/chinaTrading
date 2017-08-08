#percentile


#' get percentile of all stocks
#' @export
getPercentileAll <- function() {
  d<- fread(paste0(getTradingFolder(),"test.txt",header = FALSE))
  d<- d[,list("Ch"=V2,weekReturn=getPercentile(V1)),keyby=list(V1)]
  #names(d) <- c("Ticker", "Perc")
  return(d[, ])
}

getWtdPercentile <- function(symb) {
  weekBeginningDate <- ymd("2017-5-8")
  d <- getDataPure(symb)
  d <- d[D>= weekBeginningDate]
  if(nrow(d)>0) {
    d[, maxWeek:= max(H)]
    d[, minWeek:= min(L)]
    d[, cummax:= cummax(H)]
    d[, cummin:= cummin(L)]
    d[, perc:= (C-cummin)/(cummax-cummin)]

    return(list(perc=d[.N, perc]))
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

# get wtd percentile
getWtdMaxMin <- function(symb) {
  m<-getMondayOfWeek(Sys.Date()-1)
  print(m)
  d<- getDataPure(symb)
  d<- d[D>=m]
  if(nrow(d) > 0) {
    d[, cummax := cummax(H)]
    d[, cummin := cummin(L)]
    print(d[D>=m])
    as.list(d[.N, .(cummax,cummin,C,(C-cummin)/(cummax-cummin), (cummax+cummin)/2/C-1 )])
  } else {
    return()
  }
}

#' get week to date max and min for all stocks
#' @export
getWtdMaxMinAll <- function() {
  print("getting wtd max min all")
  print(getTradingFolder())
  res<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  res<- res[,(getWtdMaxMin(V1)),keyby=list(V1)]
  write.table(res, paste0(getTradingFolder(),"wtdMaxMin.txt"),quote = FALSE,sep = "\t", row.names = FALSE)
  res
}



#' ytd low date
#' @export
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
  d[, ytdMax:=cummax(H)]
  d[, ytdMin:= cummin(L)]
  return(list(perc=d[.N, (C-ytdMin)/(ytdMax-ytdMin)]))
  #print(d)
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