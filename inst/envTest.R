#test rcpp
library(data.table)

f<-function() {
  env <- new.env()

  l <- c("a","b","c")
  sapply(l, function(x) assign(x, rnorm(1), envir=env))

  print(env)

  d<- data.table(c(1,2,3),c("a","b","c"))
  print(d)
  d[,get(V2,env) ,keyby=.(V2)]

}

f <- function() {
  my_env <- environment()
   call_env <- parent.frame()
  # # parent.env(my_env) <- call_env
  # # print("call env ")
  # # print(call_env)
  # print(ls())
  # print("BEF")
  # print(parent.env(my_env))
  #parent.env(my_env) <- call_env
  # print("AFTER")
  # print(parent.env(my_env))



   print(parent.env(my_env))
   print(parent.frame())

   print(parent.frame(2))

   print(get(deparse(quote(y)),envir = .GlobalEnv))
   print(get("y", envir = parent.env(my_env)))
   print(get("y", envir = call_env))
   print(get("y", envir = parent.frame(1)))
   print(get("y", envir = parent.frame(2)))
   invisible()
}

y <- 3
g <- function() {
  y <- 2
  f()
}

benchList<- c("sh000001","sz399006","sz399001","sh000300","sh000016","sh000905")
benchNames <- c("主板","创","小","沪深","大","中证")
benchDt <- data.table(benchList=benchList, benchNames=benchNames)
#print(benchDt)
env <- new.env()



