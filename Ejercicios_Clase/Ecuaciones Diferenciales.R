## Método de Euler
euler <- function(f, h, x0, y0, xfinal) {
  N = (xfinal - x0) / h #Número de particiones
  x = y = numeric(N + 1) #Dimensionar el arreglo 
  x[1] = x0; y[1] = y0
  i = 1
  #Llenar los vectores
  while (i <= N) {
    x[i + 1] = x[i] + h #xi=xi-1
  y[i + 1] = y[i] + h * f(x[i], y[i]) #yi=yi-1 + h*f(xi-1,yi-1)
  i = i + 1
  }
return (data.frame(X = x, Y = y))
}
## End of the function
f <- function(t,y){cos(2*t)+sin(3*t)}
e1 <- euler(f,h=0.1,x0=0,y0=1,xfinal=0.4)
e1
plot(e1)