
#' get monday of week
getMondayOfWeek <- function(dat) {
  #require(lubridate)
  weekdayl <- list("星期一"=1,"星期二"=2,"星期三"=3,"星期四"=4,"星期五"=5,"星期六"=6,"星期日"=7 )
  if(is.Date(dat)) {
    return(dat-weekdayl[[weekdays(dat)]]+1)
  }
  return(dat)
}
