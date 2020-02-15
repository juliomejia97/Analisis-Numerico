
#Pide el tamaño de la matriz nxn
n = readline("Tamano de matriz cuadrada (nxn)  n=")
n = as.numeric(n)


#Crea la matriz
x = matrix(1:n^2, nrow = n, ncol = n)
print(x)

#Funcion para sumar la sub-matriz triangular superior
superior <- function(x, n)
{
  sup = x
  sup [lower.tri(x)] = 0
  
  print(sup)
  
  csum = colSums(sup)
  co = cumsum(csum)
  
  final = co [n:n]
  
  return(final)
  
}


#Funcion para sumar la sub-matriz triangular inferior
inferior <- function(x, n)
{
  inf = x
  inf [upper.tri(x)] = 0
  
  print(inf)
  
  rsum = rowSums(inf)
  fi = cumsum(rsum)
  
  final = fi [n:n]
  
  return(final)
  
}


#Llamado de Funciones
trianinfe = inferior(x,n)

triansup = superior(x,n)

#Analisis de Complejidad
a <- c(2:10)
b <- function(a)
{
  a*0.0001
}

plot(a, b(a), ylab="Segundos",xlab="n")
lines(a, b(a), col= "green")
