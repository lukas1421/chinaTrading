
#' #' get monday of week
#' #' @importFrom lubridate is.Date
#' getMondayOfWeek <- function(dat) {
#'   weekdayl <- list("星期一"=1,"星期二"=2,"星期三"=3,"星期四"=4,"星期五"=5,"星期六"=6,"星期日"=7 )
#'   if(is.Date(dat)) {
#'     return(dat-weekdayl[[weekdays(dat)]]+1)
#'   }
#'   return(dat)
#' }




#' get working directory
#' @export
getTradingFolder <- function() {
    return(paste0("/home/",Sys.getenv("USER"),"/Desktop/Trading/"))
    #return(paste0("C:\\Users\\",Sys.getenv("USERNAME"),"\\Desktop\\Trading\\"))
}


#' get day data folder
#' @export
getDayDataFolder <- function() {
  if(Sys.getenv("USERNAME")=="LUke") {
    mainDir <- "J:\\Data\\mainBoardR\\"
    dayDataFolder <- "J:\\TDX\\T0002\\export\\"
    minuteDataFolder <- "J:\\TDX\\T0002\\export_1m\\"
  } else if(Sys.getenv("USERNAME")=="Luke Shi") {
    mainDir <- "H:\\Data\\mainBoardR\\"
    dayDataFolder <-  "G:\\export\\"
    minuteDataFolder <-  "G:\\export_1m\\"
  }
  return(dayDataFolder)
}

#' get min data folder
#' @export
getMinuteDataFolder <- function() {
  if(Sys.getenv("USERNAME")=="LUke") {
    minuteDataFolder <- "J:\\TDX\\T0002\\export_1m\\"
  } else if(Sys.getenv("USERNAME")=="Luke Shi") {
    minuteDataFolder <-  "G:\\export_1m\\"
  }
  return(minuteDataFolder)
}

#' get main board folder
#' @export
getMainboardFolder <- function(){
  if(Sys.getenv("USERNAME")=="LUke") {
    mainDir <- "J:\\Data\\mainBoardR\\"
  } else if(Sys.getenv("USERNAME")=="Luke Shi") {
    mainDir <- "H:\\Data\\mainBoardR\\"
  }
  return(mainDir)
}

#' get cyb folder
getCYBFolder <- function() {
  if(Sys.getenv("USERNAME")=="LUke") {
    cybDir <- "J:\\Data\\cybR\\"
  } else if(Sys.getenv("USERNAME")=="Luke Shi") {
    cybDir <- "H:\\Data\\cybR\\"
  }
  return(cybDir)
}


