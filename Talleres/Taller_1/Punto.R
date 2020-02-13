#Problema 1
#Implementación de Teorema de Horner
#Polinomio: 2X⁴-3X²+3X-4 -> X = -2
horner <- function(x0,poli){
 res = 0
 for(i in 1:length(poli)){
   res = res*x0 + poli[i]
 }
 cat("El resultado del polinomio evaluado en X =",x0,"es igual a: ",res,"\n")
 return(res)
}

Q <- function(x){
  res = ((x)^51-1)/(x-1)
  cat("El resultado del polinomio evaluado en X =",x,"es igual a: ",res,"\n")
  return(res)
}
#b
poli1 = c(2,0,-3,3,-4)
horner(-2,poli1)
#c
poli2 = rep(1,50)
error = abs(horner(1.0001,poli2) - Q(1.0001))
cat("El error de cálculo, al comprarlo con la expresion es de: ",error)