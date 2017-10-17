#' helper function
#' @import zoo
#' @param symb stock
#' @param numDays rolling parameter
getMA <- function(symb,numDays) {
  label <- paste0("ma", deparse(substitute(numDays)))
  #print(label)
  dt<-getDataPure(symb)
  #z <- as.zoo(dt)
  if(nrow(dt) <= numDays) {
    return(0.0)
  }
  dt[, eval(label):=rollmean(C,numDays,fill = NA, align = "right")]
  #print(dt[.N,get(eval(label))])
  return(dt[.N, get(eval(label))])
}

#' compute MA
#' @export
#' @param numdays no. days usually 20
getMAAll <- function(numDays) {
  res<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  res<- res[,(getMA(V1,numDays)),keyby=list(V1)]
  write.table(res, paste0(getTradingFolder(),"ma20.txt"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE)
  res
}
