#Data procuring

#' general method to get stocks
#' @export
#' @import data.table
#' @param symb stock symbol sh510050
getDataPure<- function(symb) {
  #print(paste0("getting data",symb))
  #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))

  ticker <- getOneTicker(symb)

  tryCatch({
    d<- fread(paste0(getDayDataFolder(),ticker,".txt"),skip = 1,fill = T,select = c(1,2,3,4,5),
              col.names = c("D","O","H","L","C"))

    d <- d[!.N,]
    d[, D:=ymd(D)]
    return(d[,list(D,O,H,L,C)])

  },
  error=function(e) {return(data.table())})

}


#' get data
#' @export
#' @param symb stock symbol
getDataPureD<- function(symb) {
  #ticker <-
  tryCatch({
  d<- fread(paste0(getMinuteDataFolder(),getOneTicker(symb),".txt"),
            skip = 1,fill = T,select = c(1,2,3,4,5,6),key = "D",col.names = c("D","T","O","H","L","C"))
  d <- d[!.N,]
  d[, D:=ymd(D)]
  #d[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
  return(d[,list(D,T,O,H,L,C)])
  }, error=function(e) {
    return(data.table())
  })
}

#' get data
#' @export
#' @param symb stock symbol
getData <- function(symb) {
  print(paste0(" getting ",symb))
  #dataFolder <- "J:\\TDX\\T0002\\export\\"
  #ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  ticker<- getOneTicker(symb)
  #d<- fread(paste0(dataFolder,ticker,".txt"))
  d<- fread(paste0(getDayDataFolder(),ticker,".txt"),skip = 1,fill = T,col.names = c("D","O","H","L","C","V","A"))
  d <- d[!.N,]
  d[, D:=ymd(D)]
  d[, ma5:=rollapplyr(C,5,mean, fill=NA)]
  d[, ma20:=rollapplyr(C,20,mean, fill=NA)]
  d[, bull5:= ifelse(C>ma5, TRUE, FALSE)]
  d[, bull20:= ifelse(C>ma20, TRUE, FALSE)]
  d[, bull5Y:= shift(bull5,1)]
  d[, bull20Y:= shift(bull20,1)]

  if(nrow(d) > 250) {
    d[, maxAll:= rollapplyr(H,.N,max,fill=NA)]
    d[, minAll:= rollapplyr(L,.N,min,fill=NA)]
    d[, percentileAll:=(C-minAll)/(maxAll-minAll)]
    return(d[,list(D,O,H,L,C,ma5,ma20, bull5Y, bull20Y,maxAll, minAll, percentileAll)])
  } else {
    d[, maxAll:= rollapplyr(H,.N,max,fill=NA)]
    d[, minAll:= rollapplyr(L,.N,min,fill=NA)]
    d[, percentileAll:=(C-minAll)/(maxAll-minAll)]
    return(d[,list(D,O,H,L,C,ma5,ma20, bull5Y, bull20Y,maxAll,minAll, percentileAll)])
  }
}
