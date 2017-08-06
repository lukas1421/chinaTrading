#
# #All r update
# require(data.table)
# require(ggplot2)
# require(lubridate)
# require(stringr)
# require(reshape2)
# require(PerformanceAnalytics)
# require(quantmod)

if(Sys.getenv("USERNAME")=="LUke") {
  mainDir <- "J:\\Data\\mainBoardR\\"
  dayDataFolder <- "J:\\TDX\\T0002\\export\\"
} else if(Sys.getenv("USERNAME")=="Luke Shi") {
  mainDir <- "H:\\Data\\mainBoardR\\"
  dayDataFolder <-  "G:\\export\\"
}

#' generating index
fillData000001()
indexDay <- generateIndexDay()
indexM <- generateIndexMin()
resMerged <- processShcomp(indexDay,indexM)

graphShcomp(indexDay)
graphShcompD(indexM)


