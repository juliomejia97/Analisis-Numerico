#Problema 1
#Ejercicios iniciales
trunc <- function(N, dec){
  piso = floor((N*10^dec))/10^dec
  return(piso)
}
error <- function(N, dec){
  iteraciones = 0
  Real = N
  NM = 3 - dec
  
  while (Real > 1) {
    Real = Real / 10
    iteraciones =  iteraciones + 1
  }
  Aprox = trunc(Real,dec)
  err = (Real-Aprox) * (10)^3
  cat("El error de redondeo es igual a: ", round(err,2),"*10 ^",NM, " iteraciones: ",iteraciones)
}

error(536.78,4)