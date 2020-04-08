library(pracma)
library(Matrix)
library(Rlinsolve)
A = matrix(c(10,0,-1,
             -0.5,1,0.25,
             1,-0.5,1),nrow=3,byrow=TRUE)
A

B = c(0.2,-1.425,2)

diagonal1 <- function(A) {
  A[col(A)!=row(A)] <-0
  return(A)
}
propios <- function(matriz) {
  a <- eigen(matriz)#utilizar la funcion eigen
  names(a$values) <- 1:length(a$values)#genera valores
  names(a) <- c("valores","vectores")
  colnames(a$vectores) <- 1:nrow(a$vectores)
  a
}
solve(A,B)
D = diagonal1(A)

L = tril(A,k=-1)
L

U = triu(A,k=1)
U


TJ =(-solve(D))%*%(L+U)
TJ
#Valores y vectores propios
propios(matriz=y)
propios(y)
n = norm(TJ, type = "I")
n

Solucion_J <- itersolve(A,B,nmax=300,
                        tol =1e-5
                        ,method ="Gauss-Seidel")
Solucion_J
#Qué pasa si es a13=-2
A1 = matrix(c(1,0,-1,
             -0.5,1,0.25,
             -2,-0.5,1),nrow=3,byrow=TRUE)
A1
rcond(A1)
solve(A1,B)

D1 = diagonal1(A1)

L1 = tril(A1,k=-1)
L1a

U1 = triu(A1,k=1)
U1

TJ1 =(-solve(D1))%*%(L1+U1)
TJ1
#Valores y vectores propios
propios(matriz=TJ1)
propios(TJ1)
n1 = norm(TJ1, type = "I")
n1
Solucion_J1 <- itersolve(A1,B,nmax=300,
                        tol =10^(-5)
                        ,method ="Gauss-Seidel")
Solucion_J1


solucionSOR <- lsolve.sor(A,B,reltol = 1e-05,w=1.2) 
solucionSOR