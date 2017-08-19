#bench and correl

#' get bench of a stock and output to folder
#' @export
getBenchMark <- function() {
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,getCorrelGen(V1)), keyby=list(V1)]
  write.table(d, paste0(getTradingFolder(),"bench.txt"),quote = FALSE,sep = "\t")
  return(d)
}

#' correl between one stock and all benches
#' @export
getCorrelGen<-function(symb) {
  benchList<- c("sh000001","sz399006","sz399001","sh000300","sh000016","sh000905")
  dt <- data.table(benchList)
  #h <- hash()
  #h <- hash::hash(benchList, function(x) hash::.set(h,x,getDataPure(x)))
  #sapply(benchList, function(x) .set(h,x, getDataPure(x)))
  #sapply(benchList, function(x) print(h[[x]]))
  #print(h)
  #dt[, {print(benchList); print(h[[benchList]])}, keyby=list(benchList)]
  dt[, x:= getCorrel(symb,benchList), keyby=list(benchList)]
  return(list(bench=dt[order(-x)][1]$benchList,correl=dt[order(-x)][1]$x))
}


#' @export
getCorrel<- function(symb1, index) {
  #print(" index is ")
  #print(index)
  dt1<-getDataPure(symb1)
  dt1[, eval(symb1):= C/shift(C,1)-1 ]

  dt2 <- getDataPure(index)
  dt2[, eval(index):= C/shift(C,1)-1]
  dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(index))],by = "D")
  names(dt3) <- c("D",(eval(symb1)),(eval(index)))
  return(dt3[D>ymd("20120101")][!is.infinite(get(symb1)), cor(get(symb1),get(index),use="complete.obs")])
}

#' creating index
# createIndex <- function() {
#   benchList<- c("sh000001","sz399006","sz399001","sh000300","sh000016","sh000905")
#   for(i in benchList) {
#     #assign(i, getDataPure(i),envir=.GlobalEnv)
#     #assign(i, getDataPure(i),envir=as.environment("package:chinaTrading"))
#     assign(i,getDataPure(i),envir=parent.frame(1))
#   }
# }
