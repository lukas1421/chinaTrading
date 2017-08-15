tr <- getTradingHistory()

stockList<- tr[ ,list(FullTicker=unique(FullTicker))]
toTradedStocks <- stockList[, list(FullTicker)]
write.table(toTradedStocks, paste0(getTradingFolder(),"uniqueTradedStocks.txt"),quote = FALSE,sep = "\t", row.names = FALSE, append = FALSE)

# add index
indexList <- data.table(FullTicker=c("SH000001","SH000016","SH000300","SH000905","SZ399001","SZ399006"))
bindedList <- rbind(stockList,indexList)
bindedList[, t:= paste0("!",toupper(str_sub(FullTicker,1,2)),"#",str_sub(FullTicker,3),".txt")]

#day data
toGit <- bindedList[, list(t)]
write.table("*.*", paste0(getDayDataFolder(),".gitignore"), quote=FALSE,sep="\t", row.names = FALSE,col.names = FALSE,append = FALSE)
write.table(toGit, paste0(getDayDataFolder(),".gitignore"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE
            , append = TRUE)

#minute data
write.table("*.*", paste0(getMinuteDataFolder(),".gitignore"), quote=FALSE,sep="\t", row.names = FALSE,col.names = FALSE,append = FALSE)
write.table(toGit, paste0(getMinuteDataFolder(),".gitignore"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE
            , append = TRUE)
