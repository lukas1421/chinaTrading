
#' calculate sharpe of a specific stock
#' calc sharp
#' @export
calcSSSharpe <- function(symb) {
  sharpe <- calcSSSharpeDate(symb)
  print(paste0(" sharpe ", symb, " ", sharpe))
  sharpe
}

#' date
#' @export
#' @import data.table
calcSSSharpeDate <- function(symb,dat=ymd("20161231")) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>dat, mean(ret,na.rm=T)]
  sd <- d[D>dat, sd(ret,na.rm=T)]
  #print(paste0(" mean ",mean," sd ",sd))
  ytdReturn <- (d[.N,C])/(d[D<dat][.N,C])-1
  ytdMax <- d[D>dat,max(H)]
  ytdMin <- d[D>dat,min(L)]
  last <- d[.N,C]
  ytdPerc <- (last - ytdMin)/ ( ytdMax - ytdMin)
  return(list(SR=(mean/sd*sqrt(252)), mean=mean*252, sd=sd*sqrt(252),ytdRtn = ytdReturn, ytdPerc = ytdPerc))
}


#' compare all sharpe
#' @export
#' @import data.table
compareAllSharpYtd <- function() {
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,calcSSSharpe(V1)), keyby=list(V1)]
  d[, SR:= ifelse(is.na(SR),0,round(SR,1))]
  write.table(d[, list(V1,SR)], paste0(getTradingFolder(),"sharpe.txt"),
              quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)
  return(d)
}

#' compare all sharpe
#' @export
compareAllSharpYtdDate <- function(f,...) {
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,f(V1,...)), keyby=list(V1)]
  d[, SR:= ifelse(is.na(SR),0,round(SR,1))]
  write.table(d[, list(V1,SR)], paste0(getTradingFolder(),"sharpe.txt"),
              quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)
  return(d)
}

#' graph sharpe for year to date
#' @import zoo
#' @export
sharpGraph <- function(symb) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  d[, mean90d :=rollapply(ret,90, function(x) mean(x)*252,align = "right",fill=NA)]
  d[, sd:= rollapply(ret,90, function(x) sd(x)*sqrt(252),align="right",fill=NA)]
  d[, SR:= mean90d/sd]
  print(d)
  d[D>ymd("20170101")][, qplot(D,SR,geom="line")]
}

#gen last sharpe and send to TXT file


#' calculate daily mean
#' @export
calcDailyMean <- function(symb) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  return(mean)
}


#' calculate sd
#' @export
calcDailyMeanSD <- function(symb) {
  #symbName <- (substitute(symb))

  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  sd <- d[D>ymd("20161231"), sd(ret,na.rm=T)]
  return(list(symb=symb,mean=(mean),sd=sd))
}

drawRollingSD <- function(symb) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  #d[, mean(ret,na.rm=T)]
  #d[, sd:=sd(ret,na.rm=T)*sqrt(252)]
  d[, sd:= rollapply(ret,20, function(x) sd(x)*sqrt(252),align="right",fill=NA)]
  print(d[D>ymd("20161231"),list(D,sd)])
  d[D>ymd("20121231"), qplot(D,sd,geom = "line")]
}

calcDailyHLSD <- function(symb) {
  d<-getDataPure(symb)
  d[,HLret:=H/L-1]
  d[, ret:=C/shift(C,1)-1]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  sd <- d[D>ymd("20161231"), sd(ret,na.rm=T)]
  hlsd <- d[D>ymd("20161231"), sd(HLret,na.rm=T)]
  return(list(symb=symb,mean=(mean),sd=sd,hlsd=hlsd))
}






