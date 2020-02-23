#Problema 2
#Ejercicios aproximación de Taylor
taylor <- function(x, gradoP){
  resReal = sin(x)
  resAprox = 0.0
  grado = gradoP
  iteraciones = 0
  for (i in 0:grado) {
    resAprox = resAprox + ((-1)^(i)/factorial(2*i+1))*(x)^(2*i+1)
    iteraciones = iteraciones + 1
  }
  errorRelativo = abs(resReal-resAprox)
  cat("El resultado real es de: \n")
  print(resReal,16)
  cat("El resultado aproximado con Taylor es igual a: \n")
  print(resAprox,16)
  cat("El error relativo es de: ",errorRelativo, " con ",iteraciones," iteraciones")
}
taylor(-pi/128,9)
