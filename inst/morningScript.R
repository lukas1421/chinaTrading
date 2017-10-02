#morning runs
library(chinaTrading)
getFTSEData()
FTSEdataToExcel(getFTSEData())
updateFTSEWeights()
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
  srdYtd<-compareAllSharpYtd(lubridate::ymd("2016-12-31"))
  outputYtd<-srdYtd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(outputYtd[,list(V1,SR0)],
              paste0(getTradingFolder(),"sharpeOutputYtd.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)

  #
  srdQtd<-compareAllSharpYtd(lubridate::ymd("2017-6-30"))
  outputQtd<-srdQtd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(outputQtd[,list(V1,SR)],
              paste0(getTradingFolder(),"sharpeOutputQtd.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)


  #mtd
  srdMtd<-compareAllSharpYtd(lubridate::ymd("2017-8-31"))
  outputMtd<-srdMtd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(outputMtd[,list(V1,SR)],
              paste0(getTradingFolder(),"sharpeOutputMtd.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)


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

