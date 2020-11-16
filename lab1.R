# Question 1

x1<- 1/3 
x2<-1/4
if ( x1-x2==1/12) {
  print("Subtraction is correct")
} else{
  print("Subtraction is wrong")
}

x1 <- 1
x2<- 1/2
if (x1 - x2 == 1/2){
  print("Subtraction is correct")
}else{
  print("Subtraction is wrong")
}

# In the first snippet code, the result is "Subtraction is wrong", but the second snippet code the result is "Subtraction is correct". 
# The only numbers that represent exactly in r , are integers and fractions by power of 2 denominator. Hence all other
# numbers are rounded to 53 binary digits accuracy.When ever floating point operations are done , we should assume that there wil be  numeric error. 1/3 and 1/12 are repeating decimals that are rounded in R.
# You can use the all.equal function instead of == to test equality of floating point numbers.
x1 <- 1/3
x2<- 1/4
if(isTRUE(all.equal.numeric(x1-x2, 1/12))){
   print("Subtraction is correct")
 }else{
   print("Subtraction is wrong")
   
 }

# Question 2
f <- function(x) x
e <- 10^(-15)
deriv <- function(f,x,e){
  
  derivative <- (f(x+e)-f(x))/e
  return(derivative)
}
deriv(1, 100)
      