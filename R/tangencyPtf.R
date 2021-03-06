#tangency ptf

#' gen return matrix
#' @param symb stock
genReturnMatrix <- function(symb) {
  d<-getDataPure(symb)
  d[, ret:=C/shift(C,1)-1]
  res<-d[D>ymd("20161231"),list(D,ret)]
  names(res) <- c("D", symb)
  return(res)
}

#' check correl
#' @export
#' @param symb1 first stock
#' @param symb2 second stock
checkCorrel <- function(symb1,symb2) {
  d1 <- genReturnMatrix(symb1)
  d2 <- genReturnMatrix(symb2)
  res <- merge(d1,d2,by = "D")
  return(res[, cor(get(symb1),get(symb2))])
}

# ptf <- c("sz002415","sh600036","sh600660","sz000418","sh601238","sz002008","sh600519","sh600104","sz000651","sh601628")
#
# ptfSmallTest<- c("sz002415","sh600036")
#
# lapply("sh600900",calcSSSharpe)
# retMat <- matrix(nrow=10, ncol = 1)
# dimnames(retMat) <- list(ptf)
# for(i in seq_along(ptf)) {
#   retMat[i,1] <- calcDailyMean(ptf[i])
# }

generateCorMat <- function(ptf) {

  res <- data.table()
  for(x in ptf) {
    temp <- genReturnMatrix(x)
    if(length(res)==0) {
      res <- temp
    } else {
      res <- merge(res, temp,by = "D")
    }
  }

  #print(res)

  len <- length(ptf)
  corMat <- matrix(nrow = len,ncol = len)
  dimnames(corMat) <- list(ptf,ptf)
  names(corMat) <-
    for(i in seq_along(ptf)) {
      for(j in seq_along(ptf)) {
        #print(paste0("i j ", i, j, ptf[i],ptf[j]))
        if(i==j) {
          corMat[i,j] <- var(res[, get(ptf[i])])
        } else {
          corMat[i,j] <- cov(res[,get(ptf[i])], res[, get(ptf[j])])
          #print(cov(res[,get(ptf[i])], res[, get(ptf[j])]))
        }
      }
    }
  return(corMat)
}

# weightMatrix <- matrix(0.1,nrow = 10)
#
# t(weightMatrix) %*% corMat %*% weightMatrix
#
# t(weightMatrix) %*% retMat



findSharpeForTanPtf <- function(riskfree,ptf) {

  #ptf <- c("sz002415","sh600036","sh600660","sz000418","sh601238","sz002008","sh600519","sh600104","sz000651","sh601628")
  corMat<-generateCorMat(ptf)
  #print(corMat)
  sigma.inv.mat <- solve(corMat)
  one.vec <- rep(1, nrow(corMat))

  retMat <- matrix(nrow=nrow(corMat), ncol = 1)
  dimnames(retMat) <- list(ptf)
  for(i in seq_along(ptf)) {
    retMat[i,1] <- calcDailyMean(ptf[i])
  }
  print(retMat)

  rf <- riskfree

  mu.minus.rf <- retMat - rf*one.vec
  top.mat <- sigma.inv.mat%*% mu.minus.rf
  bot.val <- as.numeric(t(one.vec) %*% top.mat)
  t.vec <- top.mat[,1]/bot.val
  mean <- crossprod(t.vec,retMat)
  sd<-sqrt(as.numeric(t(t.vec) %*% corMat %*% t.vec ))
  print(t.vec)
  return(list(mean=mean,sd=sd,sr=mean/sd))
}

#exhaust

# srTable <- compareAllSharpYtd()
# srList <- unlist(srTable[V1!="sh601088"][order(-SR)][1:25][, list(V1)])
#
# ptfTry1 <- c("sz002415","sz002050","sh600900","sh600104","sh600874","sh600516","sz000069","sh600260")
# findSharpeForTanPtf(0.02,ptfTry1)

findOptimalCombo <-  function(pdf){
  #stock universe is top 100 sharpe stocks
  print(findSharpeForTanPtf(0,pdf))
}
