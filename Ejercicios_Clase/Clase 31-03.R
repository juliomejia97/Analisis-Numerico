library(pracma)
library(Matrix)
#Funciones
diagonal1 <- function(M) {
  M[col(M)!=row(M)] <- 0
  return(M)
}
propios <- function(matriz) {
  a <- eigen(matriz)#utilizar la funcion eigen
  names(a$values) <- 1:length(a$values)#genera valores
  names(a) <- c("valores","vectores")
  colnames(a$vectores) <- 1:nrow(a$vectores)
  a
}
  #Llamados
  A = matrix(c(10, 1, 0,
               -1, 10,0,
               0, 1, 10), nrow=3, byrow=TRUE)
  A
  b = c(1, 2,3)
  b
  #verificar que el sistema tenga solución
  det(A)
  sol1 <- solve(A,b)
  sol1
  #A = L + U + D
  D <- diagonal1(A)
  D
  L <- tril(A,k=-1,diag = FALSE)
  L
  U = triu(A,k=1,diag = FALSE)
  U
  D+L+U#verificación retorna A
  #Calcular la matriz de transición para Método de Jacobi
  TJ = (-solve(D))%*%(L+U)
  TJ
  #Verificar si el método converge o no
  print("Norma")
  print(norm(TJ,"I"))
  y <- TJ
  propios(matriz=y)
  propios(y)
  Solucion_J<-itersolve(A,b,nmax=3000,
                        tol = 1e-5
                        ,method = "Jacobi")
  Solucion_J