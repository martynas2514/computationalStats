

rLaplace <- function(n, mean = 0, alpha = 1){
  b <- 1/alpha
  u <- runif(n)
  res <- mean - (b*sign(u-0.5) * log(1-(2*abs(u-0.5))))
  return(res)
}

c <- seq(0,100, by = 0.1)
c <- 6
Te <- (c^2)/3
k <- 1.3
x <- (0.5 * (c^2)) / (1.5 - k)

plot(c, Te, type = "l")
lines(c,x)


test test 