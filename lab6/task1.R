# Question 1: Genetic algorithm

## 1. 

f <- function(x){
  part1 <- (x^2) / exp(x)
  expPart <- -(9*sin(x)) / ((x^2) + x + 1)
  return(part1 -2 * exp(expPart))
}

## 2.

crossover <- function(x,y){
  return((x+y)/2)
}

## 3.

mutate <- function(x){
  return((x^2)%% 30)
}

## 4. 

genetic <- function(maxiter,mutprob){
  # list for task 5 
  res <- list(initial = numeric(), final = numeric())
  
  # a)
  x <- seq(0,30, 0.1)
  plot(x,f(x), type = "l")
  
  # b)
  X <- seq(0,30,5)
  res$initial <- X
  # c)
  
  Values <- f(X)
  
  # d)
  
 for (i in 1:maxiter) {
   # i.
   parents <- sample(X,2)
   # ii.
   victim <- order(Values)[1]
   # iii.
   kid <- crossover(parents[1], parents[2])
   if(runif(1) < mutprob){
     kid <- mutate(kid)
   }
   # iv.
   X[victim] <- kid
   Values <- f(X)
   # v.
   maxX <- X[which.max(Values)]
   #points(maxX, f(maxX), col="blue", pch ="x")
 }
  res$final <- X
  points(maxX, f(maxX), col="red", pch ="x")
  return(res)
}

## 5. 
 m10m1 <- genetic(10,0.1)
 m10m5 <- genetic(10,0.5)
 m10m9 <- genetic(10,0.9)
 
 m100m1 <- genetic(100,0.1)
 m100m5 <- genetic(100,0.5)
 m100m9 <- genetic(100,0.9)
 
 res <- rbind(m10m1$initial, 
       m10m1$final, 
       m10m5$final, 
       m10m9$final, 
       m100m1$final, 
       m100m5$final, 
       m100m9$final
       )
 
 rownames(res) <- c("initial",
                    "10 0.1",
                    "10 0.5",
                    "10 0.9",
                    "100 0.1",
                    "100 0.5",
                    "100 0.9"
                    )
