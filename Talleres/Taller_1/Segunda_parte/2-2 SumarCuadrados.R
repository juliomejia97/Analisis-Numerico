n = readline("Ingrese el n para calcular la sumatoria de los primeros n^2: ")

n = as.numeric(n)

sumatoria <- function(n)
{
  cont = 0
  x <- c()
  y <- c()
  
  while(n > 0){
    
    cont = cont + n^2
    
    cuad = n^2
    
    x <- c(x, cuad)
    
    n = n - 1
    
    y <- c(y, n)
  }
  plot(x, y, xlab="cuadrado",ylab="n")
  lines(x,y, col = "green")
  
  return(cont)
}

total = sumatoria(n)
