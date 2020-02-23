#Problema 1
#Método de Horner para derivadas en x0
#Polinomio: 2X⁴-3X²+3X-4
#Función de Horner para evaluar un polinomio
horner <- function(x0,poli){
  res = 0
  for(i in 1:length(poli)){
    res = res*x0 + poli[i]
  }
  return(res)
}
#Función que deriva un polinomio  
derivar_Polinomio <- function(poli){
  derivada <- c()
  for (i in 1:length(poli)-1){
    derivada <- c(derivada,(length(poli)-i)*poli[i])
  }
  return(derivada)
}
f <- function(x) 2*x^4-3*x^2+3*x+4
g <- function(x) 8*x^3-6*x+3
plot(f,xlim = c(-5,5), ylim=c(0,20),ylab = "Y" )
par(new = TRUE)
plot(g,xlim = c(-5,5),col= "blue",ylim=c(0,20),ylab = "Y")
#Principal
poli <- c(2,0,-3,3,-4)
derivada_poli <- derivar_Polinomio(poli)
calculo<-horner(pi/14,derivada_poli)
print(calculo,16)
#Numero complejos
n1 <- complex(real = exp(1), imaginary = sqrt(3))
calculoComplejo<-horner(n1,derivada_poli)
print(calculoComplejo,16)