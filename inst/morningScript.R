#morning runs

library(chinaTrading)
getFTSEData()
getFTSE50Index()
getNAV()
getXIN0UIndex()
getIndicies()
getBOCRmbRate()

getWtdMaxMinAll()
getMAAll(20)
compareAllSharpYtd()

#sharpe
srd<-compareAllSharpYtd()
srd[order(-SR)][1:100][sd<0.3][SR>2.8]

#
getDivs()

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

