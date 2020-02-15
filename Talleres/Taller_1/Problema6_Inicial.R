#Problema 6
#Ejercicios adicionales
#Spline cúbico teórico 
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)
s1<-splinefun(x,y,method="monoH.FC",ties=mean)
plot(x,y, pch=20, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(s1(x),add = TRUE, col = 3, n = 1001)