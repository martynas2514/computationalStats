library("boot")


## 1 Plot the histogram of Price. Does it remind any conventional distribution? Compute the mean price.
prices = read.csv("C:/Users/alejo/Documents/GitHub_Repos/computationalStats/lab5/prices1.csv", sep = ";")  

hist(prices$Price, breaks = 15)

## Chi Square?

## Mean 
mean_price <- mean(prices$Price)

## 2 Estimate the distribution of the mean price of the house using bootstrap


mean_stat <- function(data, vn){
  data<- as.data.frame(data[vn, ])
  data_mean <- as.numeric(colMeans(data['Price']))
  data_mean
}

ans <- boot(data = prices, statistic = mean_stat, R=2000)
ans

plot(ans)

# CI percentil
boot.ci(ans, index = 1, type=c('perc'))

# CI bca

boot.ci(ans, index =1, type=c('bca'))

# CI normal

boot.ci(ans, index =1, type=c('norm'))

## Bias-correction

T_bias_correction <- function(data, vn)

x <- rnorm(100) ; data<-cbind(Predictor=x, Response= 3+2*x+rnorm(length(x), sd=0.5))

print(boot.ci(res))