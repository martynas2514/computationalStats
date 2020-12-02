
new_df <- read.csv("lottery.csv", sep = ";")
##View(new_df)
## 1
X <- new_df$Day_of_year
Y <- new_df$Draft_No 
plot(X,Y,main="Scatterplot Y versus X", xlab="day of year ", ylab="Draft_no ", pch=19)

## 2
# method1
lw1 <- loess(Y ~ X, data = new_df)

new_df$Y_hat <- predict(lw1, new_df$Day_of_year)


plot(Y ~ X, data=new_df,pch=19,cex=0.1)
lines(X,lw1$fitted,col="red",lwd=3)
# method2
library(ggplot2)
ggplot(new_df, aes(X, Y)) + 
  geom_point() +
  geom_smooth(method = "loess")

  
## 3
# static is loess function

statis <- function(data, model){
  index_max <- which.max(model$fitted)
  index_min <- which.min(model$fitted)
  
  X_a <- data$Day_of_year[index_min]
  Y_hat_a <- model$fitted[index_min]
  
  X_b <- data$Day_of_year[index_max]
  Y_hat_b <- model$fitted[index_max]
  statistic <- (Y_hat_b - Y_hat_a)/(X_b - X_a)
  return(statistic)
}

library(boot)
fc<- function(data, index){
  data <- as.data.frame(data[index, ])
  loes <- loess(Draft_No ~ Day_of_year, data)
  return(statis(data,loes))
}


# bootstrap
results <- boot(new_df, statistic = fc, R=2000)  # i think I made mistake here about statistics
plot(results)

p_value <- (sum(abs(results$t) > abs(statis(new_df, lw1)))/2000)


### 4 using a permutation test
permutation_test <- function(data, B){
  stat= numeric(B)
  n = dim(data)[1]
  for(b in 1:B){
    perm_data <- data.frame(data)
    perm_data$Day_of_year = sample(data$Day_of_year, n)
    loes_h1 <- loess(Draft_No ~ Day_of_year, perm_data)
    stat[b] <- statis(perm_data,loes_h1)
    
  }
  
  # statistic from original dat
  origin_loes <- loess(Draft_No ~ Day_of_year, data)
  t_origin <- statis(data, origin_loes)
  p_value <- sum(abs(stat) >= abs(statis(data, origin_loes)))/B
  #cat("t_origin",t_origin, "t_h0",mean(stat))
  return(p_value)
}
  
### 5
### 5-1
#alpha <- 0.1

new_y <- function(x, alpha){
  beta <- rnorm(1, 183, 10) 
  y <- max(0, min(alpha * x + beta, 366) )
  return(y)
}

y <- sapply(new_df$Day_of_year, new_y, alpha = 0.2)

### 5-2
new_dataset <- data.frame(Day_of_year = new_df$Day_of_year, Draft_No = y )
permutation_test(new_dataset, 200)

### 5-3
new_alpha <-seq(0.2, 10, 0.1)
p_val <- c()
for( i in 1:length(new_alpha)){
  y <- sapply(new_df$Day_of_year,new_y, alpha = new_alpha[i])
  new_dataset <- data.frame(Day_of_year = new_df$Day_of_year, Draft_No = y )
  p_val[i]<-permutation_test(new_dataset,200)
}