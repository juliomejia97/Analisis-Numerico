##### Integración Rectangular (Punto Medio)
# Limpio
rm(list = ls(all.names = TRUE))

#Definir la función
funcion.intervalo <- function(x) sin(x)
### Función de integración rectangular
### Input #1: x - Valores de x para las particiones
### Input #2: f - La función a aplicar
### Output: La integral aproximada
rectangular.integration = function(x, f)
{
  if (!is.numeric(x))
  {
    stop('No hay argumentos numéricos')
  }
  if (!is.function(f))
  {
    stop('No hay una función')
  }
  
  n.points = length(x)
  
  #Puntos medios
  puntosMedios = 0.5*(x[2:n.points] + x[1:(n.points-1)])
  
  # function evaluated at puntosMedios
  f.puntosMedios = f(puntosMedios)
  
 #Calcular el ancho de los rectangulos
  interval.widths = x[2:n.points] - x[1:(n.points-1)]
  
  #Calcular la suma de las áreas de los rectángulos
  rectangular.integral = sum(interval.widths * f.puntosMedios)
  
  #Retornar integral
  return(rectangular.integral)
}
### Función de integración trapecio
### Input #1: x - Valores de x para las particiones
### Input #2: f - La función a aplicar
### Output: La integral aproximada
trapecio.integration = function(x, f)
{
  if (!is.numeric(x))
  {
    stop('No hay argumentos numéricos')
  }
  if (!is.function(f))
  {
    stop('No hay una función')
  }
  
  n.points = length(x)
  
  #Puntos medios
  puntosMedios = 0.5*(x[2:n.points] + x[1:(n.points-1)]) * (x[2:n.points] - x[1:(n.points-1)])
  
  # function evaluated at puntosMedios
  f.puntosMedios = f(puntosMedios)
  
  #Calcular el ancho de los rectangulos
  interval.widths = x[2:n.points] - x[1:(n.points-1)]
  
  #Calcular la suma de las áreas de los rectángulos
  rectangular.integral = sum(interval.widths * f.puntosMedios)
  
  #Retornar integral
  return(rectangular.integral)
}
# Puntos para los intervalos haciendo particiones
x.support = seq(0, pi, by = 0.005)

# calcular la integral en los intervalos de la función
rectangular.integration(x.support, funcion.intervalo)