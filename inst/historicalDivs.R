library(xml2)
library(httr)
library(rvest)

url<-"http://stock.10jqka.com.cn/jyts_list/"

url<-"http://stock.10jqka.com.cn/jyts_list/index_3.shtml"
a<-read_html(url,encoding = "gbk")
l<-html_nodes(a,"a")
l1<-xml2::xml_attrs(l)
res <- (l1[which(lapply(l1,function(x) grep("沪深股市交易提示",x))>0)])
resTitle <- lapply(res, function(x) x[which(names(x)=="title")])
resHref <- lapply(res, function(x) x[which(names(x)=="href")])
#[[1]] %>%(function(x) x[which(names(x)=="href")])
lapply(resTitle,function(x) str_match(x, "([[:digit:]]{1,2})月([[:digit:]]{1,2})日"))

extractDate <- function(str) {
  str_match()
}
