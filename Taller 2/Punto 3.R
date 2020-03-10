f1 <- function(x) log(x)
curve(f1,1,2,col = 2, add = T)
x <- seq(1,2,by = (exp(1)-1)/9)
y <- f1(x)

#Diferencias divididas. diff.dv)
plot(x,y)
n<-length(y)
n
nombres<-c("f",paste("D",1:(n-1),sep="") )
diff.dv <-rep(NA,n*n)
dim(diff.dv)<-c(n,n)
diff.dv[,1]<-y
dimnames(diff.dv)<-list(0:(n-1),nombres)
for (j in 2:n) {
  for (i in 1:(n-j+1)) {
    k<-j+i-1
    print(c(i,j,k))
    diff.dv[i,j] <- (diff.dv[i+1,j-1] - diff.dv[i,j-1])/(x[k]-x[i])
  }
}
tabla<-as.matrix(data.frame(x=x,diff.dv))
print(tabla,na.print = "")
plot(x,y, col="red", xlim=c(1,3),ylim=c(0,log(4)),ylab = "Y")
#Para Xo = 1
f2<- function(x) 0.2643825*x^2+0.7788068*x - 1.19
y2 <- f2(x)
par(new =T) 
plot(x,y2,col="green", xlim=c(1,3),ylim=c(0,log(4)))
#Para x1 = 2
f3<- function(x) 0.2036406*x^2+0.6778549*x-1.3818
y3 <- f3(x)
par(new =T) 
plot(x,y3,col="blue", xlim=c(1,3),ylim=c(0,log(4)),ylab = "Y")

