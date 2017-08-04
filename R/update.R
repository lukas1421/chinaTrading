calcSharp <- function(x) {

  #ifelse(length(x[x!=0]) > 0, x<-x[x!=0], 0);

  m <- mean(x)
  cum <- tail(cumprod(1+x),1)
  max <- max(x)
  min <- min(x)
  sd <- sd(x)
  dd <- sd(x[x<m])


  dd2 <- sqrt(sum(x[x<m]^2)/length(x[x<m]))
  dd3 <- DownsideDeviation(x, MAR=m)
  ud <- sd(x[x>m])

  return(list(mean=sprintf("%.4f",mean(x)),cum = sprintf("%.4f",cum), max=sprintf("%.4f",max), min=sprintf("%.4f",min),
              sd=sprintf("%.4f",sd),ud=sprintf("%.4f",ud), dd=sprintf("%.4f",dd),dd2=sprintf("%.4f",dd2),dd3=sprintf("%.4f",dd3),
              sr = sprintf("%.4f",m/sd*sqrt(252)), sortino1=sprintf("%.4f",m/dd*sqrt(252)),sortino2=sprintf("%.4f",m/dd2*sqrt(252)),
              sortino3=sprintf("%.4f",m/dd3*sqrt(252)),sortino4=sprintf("%.4f",SortinoRatio(x)*sqrt(252))))
}



