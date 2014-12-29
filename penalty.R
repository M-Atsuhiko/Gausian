penalty <- function(N,miew,sigma){
  if(N <= 0)
    penalty <- c()
  else
    penalty <- rnorm(N,mean = miew,sd = sigma)
  return(penalty)
}
