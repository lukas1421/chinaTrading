#high low dates

#' get the high and low dates of a stock
#' @export
#' @param symb stock symbol
getHighLowDates <- function(symb) {

  #SH600036
  d <- getDataPure(symb)
  if(nrow(d) > 0) {
    tryCatch({
      lastDate <- d[.N, D]
      highDate <- d[H==max(H), ][.N][,D]
      #print(paste0(" symb :", symb," last Date: ", lastDate, " highDate: ", highDate))

      if(highDate < lastDate) {
        lowDate <- d[D>highDate,][L==min(L),][.N][,D]
      } else {
        lowDate <- 0
      }
      print(paste0(" symb :", symb," last Date: ", lastDate, " highDate: ", highDate, " low date: ", lowDate))
      return(list(symb=symb,last=lastDate,high=highDate,low=lowDate))
    },error = function(err) {
      print(paste0(" error symbol ", symb))
      print(err)
      #stock <- 0.0
      #return()
      return(list(symb="",last=0.0,high=0.0,low=0.0))
    })
  }
  return(list(symb="",last=0.0,high=0.0,low=0.0))
}

#' wrapper
#' @export
getHighLowAll <- function() {
  d<- fread(paste0(getTradingFolder(),"test.txt"),header = FALSE)
  d<- d[, c(V2,getHighLowDates(V1)), keyby=list(V1)]
  return(d)
}
