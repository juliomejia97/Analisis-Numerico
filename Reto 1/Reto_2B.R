f =function(x) sin(x)
g= function(x) cos(x)
grad = 3

x = c(NULL)
y = c(NULL)
sol = (2*(pi/64))/(grad-1)
cont1 =1
cont2 = 1


while (length(x)<grad){
  if (length(x) == 0 ){
    x[cont1]= -pi/64;
    cont1 = cont1 +1
  }
  x[cont1] = (x[cont1-1]+ (sol))
  cont1 = cont1+1
}

while (cont2 < grad+1 ){
  y[cont2]= f(x[cont2])
  cont2 = cont2+1

  }

y[cont2] = g(pi/64)

coefi = rbind(c(1,x[1],(x[1])^2,(x[1])^3),
          c(1,x[2],(x[2])^2,(x[2])^3),
          c(1,x[3],(x[3])^2,(x[3])^3),
          c(0,1,2*(pi/64),3*(pi/64)^2)
)

val=solve(coefi,y)

poli = function(x) val[1]+(val[2]*x)+(val[3]*x^2)+(val[4]*x^3)
plot(f,xlim = c(-2,2), ylim=c(-1,1),ylab = "Y" )
par(new = TRUE)
plot(poli,xlim = c(-2,2),col= "blue",ylim=c(-1,1),ylab = "Y")

puntos = pi/270

absol = abs((f(puntos)- poli(puntos)))
relat  = (absol / poli(puntos))*100
operaciones = cont1 + cont2
