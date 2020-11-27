GibsSampler <- function(n,k){
  initx <- as.data.frame(t(rep(0,k)))
  for (i in 2:n) {
    mu <- unlist(initx[i-1, ])
    mu[1] <- rnorm(1,(mu[2]+Y[1])/2,0.2/2)
    for (j in 2:(k-1)) {
      mu[j] <- rnorm(1,(mu[j-1]+mu[j+1]+Y[j])/3,0.2/3)
    }
    mu[k] <- rnorm(1,(mu[k-1]+Y[k])/2,0.2/2)
    initx <- rbind(initx,mu)
  }
  return(initx)
}