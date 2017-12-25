#update trade dates

#' update trading dates
#' @importFrom XLConnect loadWorkbook readWorksheet
#' @export
updateTradeDateFTSEOpen <- function() {
  wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"Premium and Discount_with macro.xlsm"))
  d <- XLConnect::readWorksheet(wb, sheet="2823",startRow = 400, endRow = 1000 , startCol = 1, endCol = 1,header=FALSE)
  open <- readWorksheet(wb, sheet="2823",startRow = 400, endRow = 1000 , startCol = 10, endCol = 10,header = FALSE)
  cloze <- readWorksheet(wb, sheet="2823",startRow = 400, endRow = 1000 , startCol = 11, endCol = 11,header = FALSE)
  names(d) <-c("D")
  names(open) <- c("open")
  names(cloze) <- c("close");
  e <- lubridate::ymd(d[1:nrow(open),1])
  m <- data.table(D=e,open,cloze)
  m <- m[D<Sys.Date(),][(.N-2):.N, ]
  write.table(m,paste0(getTradingFolder(),"ftseA50Open.txt"),quote = FALSE,sep = "\t",row.names = FALSE, col.names = FALSE)
  m
}


