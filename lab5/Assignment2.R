library("boot")
stat1<-function(data, vn){
data<- as.data.frame(data[vn, ])
res<-lm(Response~Predictor, data)
res$coefficients[2]
}

x <- rnorm(100) ; data<-cbind(Predictor=x, Response= 3+2*x+rnorm(length(x), sd=0.5))
res <- boot(data, stat1, R=1000)
print(boot.ci(res))