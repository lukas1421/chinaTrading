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
