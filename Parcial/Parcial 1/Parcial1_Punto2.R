secante = function(f, x0, x1, tol, maxiter = 100){
  f0 = f(x0)
  f1 = f(x1)
  k = 0
  cat("k\txn \txn+1 \t|xn-xn+1| \n")
  while (abs(x1 - x0) > tol && k <= maxiter ) {
    k = k+1
    pendiente = (f1 - f0)/(x1 - x0)
    if (pendiente == 0) return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA)
    x2 = x1 - f1/pendiente
    f2 = f(x2)
    x0 = x1; f0 = f1
    x1 = x2; f1 = f2
    # Imprimir iteraciones
    cat(k,x1, x2, abs(x1-x0), "\n")
  }
  if (k > maxiter) {
    warning("No se alcanzó el número de iteraciones")
  }
}
f = function(x) x^2-cos(x)
plot(f,-2, 2)
secante(f, -2, 0, 1e-10, 100)
secante(f, 0, 2, 1e-10, 100)