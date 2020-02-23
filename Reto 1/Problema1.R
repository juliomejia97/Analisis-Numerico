#Problema 1
#Método de Horner para derivadas en x0
#Polinomio: 2X⁴-3X²+3X-4
require(pracma)
#Función de Horner para evaluar un polinomio
horner <- function(x0,poli){
  res = 0
  for(i in 1:length(poli)){
    res = res*x0 + poli[i]
  }
  cat("El resultado del polinomio evaluado en X =",x0,"es igual a: ",res,"\n")
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
#Principal
.Machine$double.eps
poli <- c(2,0,-3,3,-4)
derivada_poli <- derivar_Polinomio(poli)
calculo<-horner(5*pi,derivada_poli)
print(calculo,16)
teorico <- fderiv(f, 5*pi) # 1ra derivada
print(teorico,16)