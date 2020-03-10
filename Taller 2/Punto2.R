ConstruirMatriz <- function(xo){
  matriz <- matrix(c(1,0,0,0,
                     1,1,1,1,
                     1,2,4,8,
                     0,1,2*xo,3*xo^2),nrow = 4,ncol = 4,byrow = TRUE)
}
#Valores esperados
x1 <- c(0,1,2)
y1 <- c(10,15,5)
A <- ConstruirMatriz(1.12)
B <- matrix(c(10,15,5,1),nrow=4,ncol=1)
sol1 <- solve(A,B)
sol1
poli <- function(x) sol1[1]+(sol1[2]*x)+(sol1[3]*x^2)+(sol1[4]*x^3)
poliDeriv <- function(x) (sol1[2])+(2*sol1[3]*x)+(3*sol1[4]*x^2)
plot(poli,xlim = c(0,2),col= "blue",ylim = c(-50,50),ylab = "Y")
par(new = TRUE)
plot(poliDeriv,xlim = c(0,2),ylim = c(-50,50),col= "red",ylab = "Y")

