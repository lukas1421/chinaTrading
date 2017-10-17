#bench and correl

#' get bench of a stock and output to folder (ticker, chn, index, correl, indexChn, indexChnCorrel)
#' @importFrom  Rcpp sourceCpp
#' @useDynLib chinaTrading
#' @export
#' @importFrom hash hash
getBenchMark <- function() {
  env <- new.env(parent = emptyenv())
  benchList<- c("sh000001","sz399006","sz399001","sh000300","sh000016","sh000905")
  benchNames <- c("主板","创","小","沪深","大","中证")
  h <- hash(benchList,benchNames)
  dt <- data.table(benchList)
  sapply(benchList,
         function(x) {
           d <- getDataPure(x)
           d[, eval(x):= C/shift(C,1)-1]
           assign(x,d, envir = env)})

  assign("b",dt,envir=env)
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,getCorrelGen(V1,env)), keyby=list(V1)]
  names(d) <- c("Ticker","chn","index","correl")
  d[, indexName:= h[[index]], keyby=list(index)]
  d[, nameCorrel:= paste0(indexName, round(correl,1) )]
  write.table(d, paste0(getTradingFolder(),"bench.txt"),quote = FALSE,sep = "\t",
              row.names = FALSE, col.names = FALSE)
  return(d)
}

#' processing a bench with four columns (output from getbenchmark) to benchlist.txt
#' this involves copying and possibly not that efficient
#' @export
#' @param d a data.table with 4 columns - ticker chnName benchTicker correl
#' @return does not return anything, but outputs a 3-col (ticker indexNameCorrel index) ()

fromBenchToBenchlist <- function() {
  benchList<- c("sh000001","sz399006","sz399001","sh000300","sh000016","sh000905")
  benchNames <- c("主板","创","小","沪深","大","中证")
  benchDt <- data.table(benchList=benchList, benchNames=benchNames)
  #print(benchDt)
  env <- new.env()
}


#' correl between one stock and all benches
#' @export
#' @param symb stock name
#' @param env environment to get variable
getCorrelGen<-function(symb,env) {
  print(symb)
  dt <- get("b", envir=env)
  dt[, x:= getCorrel(symb,benchList,env), keyby=list(benchList)]
  #print(dt)
  benchRes = dt[order(-x)][1]$benchList
  correlRes = dt[order(-x)][1]$x
  return(list(bench=benchRes,correl=correlRes))

  #return(list(bench=ifelse(!is.na(benchRes),benchRes,0.0),correl=ifelse(!is.na(correlRes),correlRes,0.0)))
  #invisible()
}

#' get correl between symb and index
#' @param symb1 stock symbol sh510050
#' @param index index sh000001
#' @param env environment in which to retrieve index
getCorrel<- function(symb1, index,env) {
  dt1<-getDataPure(symb1)

  if(nrow(dt1)==0) {
    return(0.0)
  }
  dt1[, eval(symb1):= C/shift(C,1)-1 ]
  #dt2 <- getDataPure(index)
  dt2 <- get(index, env)
  #dt2[, eval(index):= C/shift(C,1)-1]
  dt3<-merge(dt1[,list(D,get(symb1))],dt2[,list(D,get(index))],by = "D")
  names(dt3) <- c("D",(eval(symb1)),(eval(index)))

  tryCatch({
    corr <- dt3[D>ymd("20170101")][!is.infinite(get(symb1)), cor(get(symb1),get(index),use="complete.obs")]
    return(ifelse(is.na(corr),0.0,corr))
  }, error = function(e) {
    print(e)
    return(0.0)
  })
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
