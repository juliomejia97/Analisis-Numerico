raiz <- function(N){
  x = 0.1
  E = exp(-8)
  y = (1/2)*(x+(N/x))
  iter = 0
  while (abs(x-y)>E) {
    x = y
    y = y = (1/2)*(x+(N/x))
    iter = iter + 1
  }
  cat("La raiz cuadrada aproximada del numero ",N," es: ",y,"\n")
  cat("Convergencia: ",iter," iteraciones")
}
raiz(7)