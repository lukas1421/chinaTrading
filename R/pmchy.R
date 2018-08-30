

#' compute pmchy
#' @export
getPMCHY <- function() {
  res<- fread(paste0(getTradingFolder(),"indexOnly.txt"),header = FALSE)
  res<- res[,(getPMCHYFor1Stock(V1)),keyby=list(V1)]
  write.table(res, paste0(getTradingFolder(),"pmchy.txt"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE)
  res
}

#' compute 1 stock pmch
#' @export
getPMCHYFor1Stock <- function(symb) {
  dt<-getDataPureD(symb)
  lastDate <- dt[.N,D]
  dt[D==lastDate, .(max(H),min(L), O[T==1301],C[T==1500]) ]
  maxMin <- dt[D==lastDate, .(D=D[T==1500],max=max(H),min=min(L), pmOpen=O[T==1301],close=C[T==1500]) ]
  maxMin[, pmch:=round((close-pmOpen)/(max-min)*100) ]
  maxMin[, closeP:=round((close-min)/(max-min)*100) ]
  #print(maxMin)
  return(maxMin)
}

