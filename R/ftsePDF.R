#' download FTSE index constituents sheet
#' @importFrom pdftools pdf_text
#' @import data.table
#' @import stringr
#' @import XLConnect
#' @import rvest
#' @export
#' @author Lucas
getFTSEData <- function() {

  download.file("https://www.ftse.com/analytics/factsheets/Home/DownloadConstituentsWeights/?indexdetails=XINA50"
                , destfile = paste0(getTradingFolder(),"res.pdf"), mode="wb")

  toc <- pdf_text(paste0(getTradingFolder(),"res.pdf"))

  #print(toc)
  t1<-str_split(toc[1],"\n")

  res <- data.table(rep(0,50),rep(0,50))
  names(res) <- c("stock","weight")

  j<-1

  for(i in seq(7,24,1)) {
    lapply(str_split(str_trim(str_split(t1[[1]][i],"CHINA")[[1]],"right"), "\\s\\s+"),
           function(x) {
             if(str_trim(x, side="left")[[1]] !="") {
               if(length(str_trim(x, side="left")) == 2) {
                 res$stock[j] <<- str_trim(toupper(str_replace(str_trim(x, side="left")[[1]],"\\(.*\\)","")))
                 res$weight[j] <<- as.numeric(str_trim(x, side="left")[[2]])
                 j <<- j+1
               } else if (length(str_trim(x, side="left")) == 3) {
                 res$stock[j] <<- str_trim(toupper(str_replace(str_trim(x, side="left")[2],"\\(.*\\)","")))
                 res$weight[j] <<- as.numeric(str_trim(x, side="left")[3])
                 j <<- j+1
               }
             }
           })
  }
  wb <- XLConnect::loadWorkbook(paste0(getTradingFolder(),"new.xlsx"),create = TRUE)
  createSheet(wb,"Sheet1")
  XLConnect::writeWorksheet(wb,res,"Sheet1",startRow = 1,startCol = 1, header = T)
  XLConnect::saveWorkbook(wb)
}

updateFTSEWeights <- function() {
  res <- getFTSEData()
  wb <- loadWorkbook(paste0(getTradingFolder(),"new.xlsx"),create = TRUE)
  createSheet(wb,"Sheet1")
  writeWorksheet(wb,res,"Sheet1",startRow = 1,startCol = 1, header = T)
  saveWorkbook(wb)
}

#################################################################################################################################################################################

#' getting NAVs
#' @export
#' @import xml2
#' @import stringr
#'
getNAV <- function() {
  stocks <- c("2823:HK","2822:HK", "3147:HK", "3188:HK", "FXI:US","CNXT:US","ASHR:US","ASHS:US")
  for(i in stocks) {
    print(i)
    #a <- read_html(httr::GET(paste("https://www.bloomberg.com/quote/",i, sep=""),use_proxy("127.0.0.1",1080)))
    a <- read_html(httr::GET(paste("https://www.bloomberg.com/quote/",i, sep="")))
    b <- html_nodes(a,"div") %>% html_text() %>% (function(x) {x[str_sub(str_trim(x),1,3) == "NAV"]})
    b<-(str_split(b[[1]],"\\s\\s+"))
    c<-as.numeric(str_match(html_nodes(a,"meta")[str_detect(rvest::html_nodes(a,"meta"),"price")][1],"[[:digit:].]+"))
    d<-as.numeric(b[[1]][3])
    print(paste('price',c,'NAV',d,'prem/disc',sprintf("%.2f%%", 100*round(10000*(c/d-1))/10000)))
  }
}
########################################################################################################################################################################

#' getting ftse A50 index
#' @export
getFTSE50Index <- function (){
  a <- read_html("https://hk.investing.com/indices/ftse-china-a50")
  a<- (html_nodes(a,"td") %>% (function(x) {x[str_detect(x,".*28930.*")]}) %>% html_text())
  a <- as.numeric(str_replace(a,"[,|%]",""))
  return(c(a[1],a[2],a[1]-a[2],sprintf("%.2f%%", 100*log(a[1]/(a[1]-a[2])))))
}


########################################################################################################################################################################

#' XIN0U
#' @export
getXIN0UIndex <- function() {
  a <- read_html("http://finance.yahoo.com/quote/XIN0UN.FGI?ltr=1")
  price <-  html_nodes(a,"span") %>% (function(x) {x[str_detect(x,"36px.*4px")]}) %>% html_text()
  chg <- html_nodes(a,"span") %>% (function(x) {x[str_detect(x,"10px.*24px")]}) %>% html_text()
  chg <- str_match(chg, "^[+-][:digit:]+\\.[:digit:]+")
  #print(chg)
  price <- as.numeric(str_replace(price,"[,|%]",""))
  yest <- as.numeric(price)-ifelse(str_sub(chg,1,1)=="+", 1,-1)*abs(as.numeric(chg))
  return(c(price, chg[1],yest,sprintf("%.2f%%",100*log(price/yest))))
}

########################################################################################################################################################################

#' get Index
#' @export
#' @importFrom  xml2 read_xml

getIndicies <- function() {
  indices <- c("sh000001","sz399006","sh000300","sh000905",'sh000016')
  for (i in indices) {
    #print(guess_encoding())
    a <- read_html(x = paste0("http://hq.sinajs.cn/list=",i))
    a <- a %>% html_text()
    a <- str_split(a, ",")
    print(paste(i, ": ",a[[1]][4], " Previous " , a[[1]][3], " ch ", sprintf("%.2f%%",100*log(as.numeric(a[[1]][4])/as.numeric(a[[1]][3])))))
  }
}

#getIndicies()
##########################################################################################################################################

#' daily shcomp
#' @export
getSHCOMP <- function() {


  AMOPENT = 931
  AMCLOSET = 1129
  PMOPENT = 1300
  CLOSET = 1500

  #minuteData <- fread("C:\\Users\\LUke\\Desktop\\Trading\\shcompDate.txt")

  minuteData <- fread(paste0(getMinuteDataFolder(),"SH#000001.txt"), skip = 1, fill=T, showProgress = T, col.names =c("D","T", "O","H","L","C","V","A") )
  minuteData <- minuteData[!.N,]
  minuteData[, D:=ymd(D)]
  minuteData<-minuteData[D==D[.N],list(D,T,O,H,L,C)]

  res <- minuteData[, list("AmOpen"=O[T==AMOPENT], "931"=C[T==AMOPENT], "935"=C[T==935], "940"=C[T==940], "AmClose"=C[T==AMCLOSET], "AmMax" = max(H[T<1200]), "AmMin"=min(L[T<1200]),
                           "AmMaxT" = T[T<1200][which.max(H[T<1200])], "AmMinT" = T[T<1200][which.min(L[T<1200])], "PmOpen"=O[T==PMOPENT], "Pm1310"=C[T==1310],"PmClose"=C[T==1459],
                           "PmMax" = max(H[T>1259]), "PmMin"=min(L[T>1259]), "PmMaxT" = T[T>1259][which.max(H[T>1259])], "PmMinT" = T[T>1259][which.min(L[T>1259])]
  ), ]

  print(res)
  write.table(res,paste0(getTradingFolder(),"shcomp.txt"),quote = FALSE,sep = "\t",row.names = FALSE)

}
#require(lubridate)
#getSHCOMP()


###########################################################################################################################################
#' get BOC rmb rate
#' @export
getBOCRmbRate<- function(){
  a <- read_html("http://www.boc.cn/sourcedb/whpj")
  a <- html_nodes(a, "tr")
  a <- html_text(a)
  a <-iconv(a,"utf-8","gb2312")

  a<-str_split(str_trim(a[str_detect(a,"美元")]),"\\s+")
  a[[1]] <- NULL
  a <- data.table(matrix(unlist(a),nrow = length(a),byrow = T))
  #names(a) <- c("货币名称","现汇买入价"," 现钞买入价","现汇卖出价","现钞卖出价","中行折算价","发布日期","发布时间")
  names(a) <- c("currency","Buy wire"," Buy cash","Sell wire","Sell Cash","BOC","Date","Time")
  #a[, BOC:=as.numeric(BOC)/100]
  print(as.numeric(a$BOC)/100)
  print(a)
}

#getBOCRmbRate()

