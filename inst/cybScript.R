#cyb update script
if(Sys.getenv("USERNAME")=="LUke") {
  mainDir <- "J:\\Data\\mainBoardR\\"
  dayDataFolder <- "J:\\TDX\\T0002\\export\\"
} else if(Sys.getenv("USERNAME")=="Luke Shi") {
  mainDir <- "H:\\Data\\mainBoardR\\"
  dayDataFolder <-  "G:\\export\\"
}

fillData399006()
cybIndex<-generateCYBDay()
cybMin<-generateCYBMin()
cybMerged<-process(cybIndex, cybMin)
graphCYB(cybIndex)
graphCYBD(cybMin)
