#morning runs

library(chinaTrading)
getFTSEData()
getFTSE50Index()
getNAV()
getXIN0UIndex()
getIndicies()
getBOCRmbRate()

# maxmin/ma/sharpe
getWtdMaxMinAll()
getMAAll(20)
compareAllSharpYtd()

#sharpe
srd<-compareAllSharpYtd()
srd[order(-SR)][1:100][sd<0.3][SR>2.8]

#get divs
getDivs()


# bench CAUTIOUS LONG EXECUTION
#
getBenchMark()


#bench & benchlist
# Daily task
#ftse
#getFTSEData()
#getFTSE50Index()
#getNAV()
#getXIN0UIndex()
#getIndicies()
#getBOCRmbRate()


# wtd max min
#getWtdMaxMinAll()

#ma
#getMAAll(20)

#divs
#getDivs()

#sharpe
#compareAllSharpYtd()

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

