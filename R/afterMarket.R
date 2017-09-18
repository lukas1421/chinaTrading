#' #after market
#' #fill up tradeList
#'
#'
#'
#' #' filling up trade list after market (flavor 1 is getting feed from trading tool)
#' #' @export
#' #' @import XLConnect
#' fillTradeList1 <- function() {
#'
#'   prices <- fread(paste0(getTradingFolder(),"pricesTodayYtd.csv"), fill=T, col.names =c("T","Price","Previous",""))
#'
#'   prices <- prices[-"V1",]
#'   wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"Trade List.xlsm"),create = TRUE)
#'   XLConnect::writeWorksheet(wb,prices,sheet = "Prices",startRow = 2,startCol = 1, header = T)
#'   XLConnect::saveWorkbook(wb)
#' }
#'
#' #' filling up trade list after market (flavor 2 is getting from sina)
#' #' @export
#' #' @import XLConnect
#' fillTradeList1 <- function() {
#'
#'   wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"tradeList.xlsx"),create = TRUE)
#'   XLConnect::writeWorksheet(wb,res,"Sheet1",startRow = 1,startCol = 1, header = T)
#'   XLConnect::saveWorkbook(wb)
#'
#' }
