#' t costs
#' @export

computeMonthlyTcost <- function() {

  t <- data.table::fread(paste0(getTradingFolder(),"tcost.csv"),header = T, fill = T, skip = 0)
  t[, Date:= ymd(Date)]
  t[, firstDate:= Date - (day(Date)-1), keyby=list(Date) ]

  print(t[, .(Guohu=sum(Guohu),Bro=sum(Brokerage),Stamp=sum(Stamp), Total=sum(Guohu+Brokerage+Stamp))])
  print(t[, .(Guohu=sum(Guohu),Bro=sum(Brokerage),Stamp=sum(Stamp), Total=sum(Guohu+Brokerage+Stamp)), keyby=list(firstDate) ])


  return()
}


getMonthFirstDay <- function(date) {

}

