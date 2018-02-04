#monthly sharpe

#' monthly sharpe, month, mean, sr
#' @export
#' 
getMonthlySharpe <- function(symb) {
  d <- getDataPure(symb)
  d<-d[month(shift(D,1,type = "lead"))!=month(D)]
  d[, monthRet:= C/shift(C,1)-1]
  return(d[!is.na(monthRet),calcSharp(monthRet), keyby=month(D)][, .(month,mean,sr)][order(-as.numeric(sr))])
}