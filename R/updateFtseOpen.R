#update trade dates

#' update trading dates
#' @importFrom XLConnect loadWorkbook readWorksheet
#' @export
updateTradeDateFTSEOpen <- function() {
  wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"Premium and Discount_with macro.xlsm"))
  d <- XLConnect::readWorksheet(wb, sheet="2823",startRow = 250, endRow = 1000 , startCol = 1, endCol = 1)
  open <- readWorksheet(wb, sheet="2823",startRow = 250, endRow = 1000 , startCol = 10, endCol = 10)
  names(d) <-c("D")
  names(open) <- c("open")
  m <- data.table(D=as.Date(d[1:nrow(open),1]),open)
  m <- m[D<Sys.Date(),][(.N-2):.N, ]
  write.table(m,paste0(getTradingFolder(),"ftseA50Open.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names = FALSE)
  m
}


