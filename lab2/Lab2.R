interpolator <- function(a, x){
  X <- as.matrix(c(1, x, x^2), ncol = 1 )
  return(as.vector(a %*% X))
}

SSE <- function(x, a, method1, method2){
  real <- sapply(x, method1)
  pred <- sapply(x, method2, a = a)
  return(sum((real-pred)^2))
}

optimiser <- function(x, method){
  aInit <- rep(1,3)
  res <- optim(aInit, SSE, x = x, method1 = method, method2 = interpolator)
  return(res$par)
}

aproximate <- function(n, method){
  scale <- 1/n
  midVal <- scale / 2
  
  # value1-----value2------value3
  result <- list()
  for (i in 1:n) {
    x <- c((scale*i)-scale, (scale * i) - midVal, scale * i)
    result <- append(result, list(optimiser(x, method)))
  }
  return(result)
}


f1 <- function(x){
  return(-x * (1-x))
}

f2 <- function(x){
  return(-x * sin(10 * pi * x))
}


x <- seq(0, 1, by = 0.01)
plot(x,f1(x))

test <- aproximate(100, f1)
scale <- 1/length(test)
for (i in 1:length(test)) {
  x <- seq((scale*i)- scale, scale*i, by = 0.01/length(test))
  yint <- sapply(x, interpolator, a = test[[i]])
  lines(x,yint)
}
