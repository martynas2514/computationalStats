###1
### function for target density###
target <- function(x){
    return((x^5)*exp(-x))}


### density of Log-normal function by sigma=1
##dlnorm gives the density
proposed <- function(x, mu){
  res <- dlnorm(x,log(mu), sd=1)
  return(res)
}
### rlnorm generates random deviates.
metro_hast <- function(startvalue, iteration){
  stored <- rep(startvalue, iteration)  
  #x0 <- 0.001
  for (i in 2:iteration){  ### I am not sure about 2
    x <- stored[i-1]
    xprime <- rlnorm(1,log(x),1)  # proposed a value for start and I am not sure about x=1
    ratio <- min(c(1,target(xprime) * proposed(x, xprime)) / (target(x) * proposed(xprime, x)))
    accept <- (runif(1) <= ratio)
    stored[i] <- ifelse(accept, xprime, x)
  }
  #return(stored)
  #plot(stored, type="l")
  hist(stored[2000:10000] , breaks = 30 )
}
metro_hast(100,10000)
##2
####chi- square distribution
##The dchisq() function gives the density
p_chi_square <- function(x, df){
  return(dchisq(x, df=floor(x+1)))
}
metro_chi_hast <- function(startvalue, iteration){
  stored <- rep(startvalue, iteration)  
  vN<-1:iteration
  for (i in 2:iteration){  ### I am not sure about 2
    x <- stored[i-1]
    xprime <- rchisq(1,df=floor(x+1))  # proposed a value for start and I am not sure about x=1
    ratio <- min(c(1,target(xprime) * p_chi_square(x, xprime)) / (target(x) * p_chi_square(xprime, x)))
    accept <- (runif(1) <= ratio)
    stored[i] <- ifelse(accept, xprime, x)
  }
  #return(stored)
  #plot(stored, type="l")
  hist(stored[5000:10000] , breaks = 30 )
  #plot(vN,stored,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(x-0.5,-5),max(5,x+0.5)))
}
metro_chi_hast(100,10000)


#### 4 
### Generate 10 MCMC
library(coda)
k <- 10
f1 <- mcmc.list()
for(i in 1:k){
  f1[[i]]<-as.mcmc(metro_chi_hast(i, 10000))
}
print(gelman.diag(f1))