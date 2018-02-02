#historical high time

#' get the hist high date (earliest match, conservative)
#' @export
getHistHighDate <- function(symb) {
  print(symb)
  dt<-getDataPure(symb)
  if(nrow(dt)>0) {
    h <- dt[, max(H)]
    return(dt[H==h, D][1])
  } else {
    return(lubridate::ymd("19991231"))
  } 
}


#' get hist high for all stocks, wrapper function
#' @export
#' 
getHistHighDateAll <- function() {
  res<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  res<- res[,(getHistHighDate(V1)),keyby=list(V1)]
  names(res) <- c("ticker","date1")
  res <- res[order(-date1)]
  write.table(res, paste0(getTradingFolder(),"histHighDate.txt"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE)
  res
}