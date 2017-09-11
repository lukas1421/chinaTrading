#morning runs
library(chinaTrading)
getFTSEData()
getFTSE50Index()
getNAV()
getXIN0UIndex()
getIndicies()
getBOCRmbRate()
updateTradeDateFTSEOpen()
getSumSumSqAll(Sys.Date())

# maxmin/ma/sharpe
getWtdMaxMinAll()
getMAAll(20)

#sharpe
#compareAllSharpYtd()
local({
  srd<-compareAllSharpYtd(lubridate::ymd("2016-12-31"))
  output<-srd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(output[,list(V1,SR)],
              paste0(getTradingFolder(),"sharpeOutput.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)
})

# bench CAUTIOUS LONG EXECUTION
getBenchMark()

# srd1 <- compareAllSharpYtd(ymd("2014-1-30"))
# srd1[order(-SR)][1:100][sd<0.3][SR>0]
#weekly sharp
#get divs

getDivs()

#
saveToGit()

#creating index for comparison, get bench
#createIndex()
#d<-getBenchMark()
#' #' run daily tasks
#' #' @export
#' #Methods only:
#' runMorningTasks <- function() {
#'   getFTSEData()
#'   getFTSE50Index()
#'   getNAV()
#'   getXIN0UIndex()
#'   getIndicies()
#'   getBOCRmbRate()
#' }
#'
#' #' wrapper
#' #' @export
#' getWtdMaxMinFn <- function() {
#'   getWtdMaxMinAll()
#' }
#' #' get week max/min + ma
#' #' @export
#' getMaxMinMA <- function() {
#'   getMAAll(20)
#' }
#'
#' #' get sharpe
#' #' @export
#' #compute ytd sharpe and output to file
#' computeSharp <- function() {
#'   compareAllSharpYtd()
#' }

