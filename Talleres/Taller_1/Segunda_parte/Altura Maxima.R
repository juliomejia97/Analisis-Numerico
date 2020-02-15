altmax <- function(fun, t){
  cont = 2
  max = 0
  
  m <- c(1:39)
  
  for (i in m){
    
    aux1 = fun[cont:cont]
    aux2 = fun[(cont + 1):(cont + 1)]
    
    if (aux1 < aux2){
      max = fun[cont:cont]
      cont = cont + 1
      
    }
    else {
      cont = cont + 1
  }
    
    
  }
  return = max
  
}

t <- c(1:41)
fun = 6+2.13*t^2-0.0013*t^4

plot(t,fun, pch=20, col = "blue",xlab="Tiempo", ylab="Altura")
lines(t,fun, col = "green")

maxi = altmax(fun,t)

print(maxi)
