##### Integraci�n Rectangular (Punto Medio)
# Limpio
rm(list = ls(all.names = TRUE))

#Definir la funci�n
funcion.intervalo <- function(x) sin(x)
### Funci�n de integraci�n rectangular
### Input #1: x - Valores de x para las particiones
### Input #2: f - La funci�n a aplicar
### Output: La integral aproximada
rectangular.integration = function(x, f)
{
  if (!is.numeric(x))
  {
    stop('No hay argumentos num�ricos')
  }
  if (!is.function(f))
  {
    stop('No hay una funci�n')
  }
  
  n.points = length(x)
  
  #Puntos medios
  puntosMedios = 0.5*(x[2:n.points] + x[1:(n.points-1)])
  
  # function evaluated at puntosMedios
  f.puntosMedios = f(puntosMedios)
  
 #Calcular el ancho de los rectangulos
  interval.widths = x[2:n.points] - x[1:(n.points-1)]
  
  #Calcular la suma de las �reas de los rect�ngulos
  rectangular.integral = sum(interval.widths * f.puntosMedios)
  
  #Retornar integral
  return(rectangular.integral)
}
### Funci�n de integraci�n trapecio
### Input #1: x - Valores de x para las particiones
### Input #2: f - La funci�n a aplicar
### Output: La integral aproximada
trapecio.integration = function(x, f)
{
  if (!is.numeric(x))
  {
    stop('No hay argumentos num�ricos')
  }
  if (!is.function(f))
  {
    stop('No hay una funci�n')
  }
  
  n.points = length(x)
  
  #Puntos medios
  puntosMedios = 0.5*(x[2:n.points] + x[1:(n.points-1)]) * (x[2:n.points] - x[1:(n.points-1)])
  
  # function evaluated at puntosMedios
  f.puntosMedios = f(puntosMedios)
  
  #Calcular el ancho de los rectangulos
  interval.widths = x[2:n.points] - x[1:(n.points-1)]
  
  #Calcular la suma de las �reas de los rect�ngulos
  rectangular.integral = sum(interval.widths * f.puntosMedios)
  
  #Retornar integral
  return(rectangular.integral)
}
# Puntos para los intervalos haciendo particiones
x.support = seq(0, pi, by = 0.005)

# calcular la integral en los intervalos de la funci�n
rectangular.integration(x.support, funcion.intervalo)