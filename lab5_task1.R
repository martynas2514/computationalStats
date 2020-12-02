setwd("D:/Linkoping university/first semester/Computational Statistics/labs/Labs/Lab5")
new_df <- read.csv("lottery.csv", sep = ";")
View(new_df)
## 1
X <- new_df$Day_of_year
Y <- new_df$Draft_No 
plot(X,Y,main="Scatterplot Y versus X", xlab="day of year ", ylab="Draft_no ", pch=19)

## 2
# method1
lw1 <- loess(Y ~ X, data = new_df)

new_df$Y_hat <- predict(lw1, new_df)
new_df

plot(Y ~ X, data=new_df,pch=19,cex=0.1)
j <- order(X)
lines(X[j],lw1$fitted[j],col="red",lwd=3)
# method2
library(ggplot2)
ggplot(new_df, aes(X, Y)) + 
  geom_point() +
  geom_smooth(method = "loess")

  
## 3
# static is loess function 
library(boot)
fc<- function(data, index){
  d2 <-data[index,]
  loes <- loess(Draft_No ~ Day_of_year, data)
  X_b <- which.max(loes$fitted)
  X_a <- which.min(loes$fitted)
  Y_hat_a <- predict(loes, X_a)  or loes$fitted[X_b]
  Y_hat_b <- predict(loes, X_b)  or loes$fitted[X_a]
  
  statistic <- (Y_hat_b - Y_hat_a)/(X_b - X-a)
  return(statistic)
}
# bootstrap
results <- boot(new_df, statistic = statistic, R=2000)  # i think I made mistake here about statistics
plot(results)
?boot
## p_value:
mean_boot <- results$t - mean(results$t)   # i am not sure about the formule of p value
p-value <- mean(abs(mean_boot)) >= abs(results$t0)

### 4 using a permutation test
pernuation_test <- function(data, B){
  stat= numeric(B)
  n = dim(new_df)[1]
  for(b in 1:B){
    # statistic from sample data    slide 13 from lecture
    # add new column for sample of X
    new_df$Gb = sample(new_df$Day_of_year, n)
    loes_h1 <- loess(Y ~ Gb, new_df)
    X_b <- which.max(loes_h1$fitted)
    X_a <- which.min(loes_h1$fitted)
    Y_hat_a <- predict(loes_h1, X_a)
    Y_hat_b <- predict(loes_h1, X_b)
    stat[b] <- (Y_hat_b - Y_hat_a)/(X_b - X_a)
    
  }
  
  # statistic from original dat
  origin_loes <- loess(Y ~ X, new_df) 
  X_B <- which.max(origin_loes$fitted)
  X_A <- which.min(origin_loes$fitted)
  Y_hat_A <- predict(origin_loes, X_A)
  Y_hat_B <- predict(origin_loes, X_B)
  stat0 <- (Y_hat_B - Y_hat_A)/(X_B - X_A)
  p_value <- mean((stat) >= stat0)   ### slide13 of lecture
  return(p_value)
}
  
### 5
### 5-1
beta <- rnorm(x, 183, 10)   ### about x ???
#alpha <- 0.1
new_dataset <- as.data.frame()
new_y <- function(alpha){
  for( i in lenght(new_df$Day_of_year)){
  new_dataset$new_draft[i] <- max(0, min(alpha * new_df$Day_of_year[i] + beta), 366)}
  return(new_draft)
}

### 5-2
pernuation_test(new_dataset, 200)

### 5-3
new_alpha <-seq(0.2, 10, 0.1)
p_val <- c()
for( i in new_alpha){
  new_dataset <- as.data.frame() ### ???? I am not sure to put or not
  n_y <- new_y(i)
  p_val[i]<-pernuation_test(new_dataset,200)
  
  
}