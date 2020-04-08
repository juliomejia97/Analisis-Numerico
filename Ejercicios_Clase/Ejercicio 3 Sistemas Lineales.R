library(pracma)
library(Matrix)
library(Rlinsolve)
#Declaración de funciones
#Funcion del virus
virus <- function(k1,k2,k3,t) k1*t + k2*t^2 + k3*exp(0.15*t)
#Crear Diagonal de una matriz
diagonal1 <- function(A) {
  A[col(A)!=row(A)] <-0
  return(A)
}
#Valores y vectores propios de una matriz
propios <- function(matriz) {
  a <- eigen(matriz)#utilizar la funcion eigen
  names(a$values) <- 1:length(a$values)#genera valores
  names(a) <- c("valores","vectores")
  colnames(a$vectores) <- 1:nrow(a$vectores)
  a
}
A = matrix(c(10,10^2,exp(0.15*10),
             15,15^2,exp(0.15*15),
             20,20^2,exp(0.15*20)),nrow=3,byrow=TRUE)
A
B = c(25,130,650)
#Verificar si mediante el método solve existe una solución
S1 <- solve(A,B)
#Crear la diagonar, triangulas superior e inferior
D = diagonal1(A)
#Superior
L = tril(A,k=-1)
L
#Inferior
U = triu(A,k=1)
U
#Matriz de transición para verificar la convergencia
TJ =(-solve(D))%*%(L+U)
TJ
#Norma de la matriz de transición, si es mayor que 1 no se asegura la convergencia
n = norm(TJ, type = "I")
n
#Valores y vectores propios
propios(matriz=A)
propios
#Apartir de los vectores propios y ver hay una parte imaginaria, es posible ver que sí puede existir una convergencia
#Se soluciona mediante el método de Gauss-Seidel
Solucion_J <- itersolve(A,B,nmax=300,
                        tol =1e-5
                        ,method ="Gauss-Seidel")
Solucion_J
#Calculo del error entre Gauss-Seildel y Solve
abs(Solucion_J$x - S1) / S1
#Aplicando el método de SOR con w=1.6
solucionSOR <- lsolve.sor(A,B,reltol = 1e-5,w=1.2)
solucionSOR$x
solucionSOR$iter
abs(solucionSOR$x - S1) / S1
#La aproximación que se utilizará será Gauss Seidel, ya que es la que menor error tiene con respecto a Solve
personas1 <- virus(Solucion_J$x[1],Solucion_J$x[2],Solucion_J$x[3],1500)
personas1
personas2 <- virus(Solucion_J$x[1],Solucion_J$x[2],Solucion_J$x[3],1800)
personas2
personas3 <- virus(Solucion_J$x[1],Solucion_J$x[2],Solucion_J$x[3],2000)
personas3