library(pracma)
library(numDeriv)
#Punto i
s<- function(x) x*exp(x)
r = 5.436563656918091
h = 0.1
valores <- c()
precision <- c()
for (k in 1:15){
  d = (s(1+h)-s(1))/h
  e = abs(r-d)
  valores <-c(valores, h)
  precision <- c(precision,e)
  h = h/10
}
print(valores)
print(precision)
plot(valores,precision)
#Punto j
i <- function(i) i
f <- function(L,R,vi) L*fderiv(f = i,x = vi,h = 10^-6) + R*vi
x <- c(1,1.01,1.02,1.03,1)
vi <- c(3.10,3.12,3.14,3.18,3.24)
y <- c()
for (j in 1:5){
  y <- c(y,f(0.98,0.142,vi[j]))
  print(y[j])
}
plot(x,y)