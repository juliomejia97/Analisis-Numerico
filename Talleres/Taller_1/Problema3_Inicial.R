#Problema 3
#Ejercicios iniciales
taylor <- function(){
  resReal = exp(0.5)
  resAprox = 0.0
  grado = 5
  x = 0
  for (i in 0:grado) {
    resAprox = resAprox + (((exp(x))*(0.5^i))/factorial(i))
  }
  cat("El resultado real es igual a ",round(resReal,5),"\n")
  cat("El resultado aproximado con Taylor es igual a ",round(resAprox,5),"\n")
}
taylor()