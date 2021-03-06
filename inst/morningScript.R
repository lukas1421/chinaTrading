#morning runs
library(chinaTrading)
#getFTSEData()

local({
  #res <- getFTSEData()
  system("proxychains wget --output-document ~/Desktop/Trading/res.pdf  https://www.ftse.com/analytics/factsheets/Home/DownloadConstituentsWeights/?indexdetails=XINA50")
  #system("cd ~/Downloads && ls | grep 'XINA50' | tail -1 | xargs -I {} mv {} res.pdf")
  #system("mv ~/Downloads/res.pdf ~/Desktop/Trading")
  res <- getFTSEDataNoDownload()
  FTSEdataToExcel(res)
  updateFTSEWeights(res)
})



#not in use
updateTradeDateFTSEOpen()
getPMCHY()

getSumSumSqAll(Sys.Date())
#maxmin/ma/sharpe
getWtdMaxMinAll()
getMAAll20()
getMAAll60()

getHistHighDate()
#sharpe
compareAllSharpYtd(lubridate::ymd("2017-12-31"))

local({
  srdYtd<-compareAllSharpYtd(lubridate::ymd("2016-12-31"))
  outputYtd<-srdYtd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(outputYtd[,list(V1,SR)],
              paste0(getTradingFolder(),"sharpeOutputYtd.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)

  #
  srdQtd<-compareAllSharpYtd(lubridate::ymd("2016-12-31"))
  outputQtd<-srdQtd[order(-SR)][1:100][sd < 0.3][SR>2.8]
  write.table(outputQtd[,list(V1,SR)],
              paste0(getTradingFolder(),"sharpeOutputQtd.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names =FALSE)


  #mtd
  srdMtd<-compareAllSharpYtd(lubridate::ymd("2017-12-31"))
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

#
getNAV()
getFTSE50Index()
getXIN0UIndex()
getIndicies()
getBOCRmbRate()

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

print(" TASK DONE")
