
#Declaracion de paquetes y librerias usadas
install.packages("tidyverse")
library(tidyverse)
library(stringr)


#Funcion para pasar enteros de decimal a binarios
bin <- function(nume) 
{
  base = 0
  exp = 1
  while (nume > 0) 
  {
    digito = nume %% 2
    nume = floor(nume / 2)
    base = base + digito * exp
    exp = exp * 10
  }
  return(base)
}

#Funcion para pasar partes fraccionarias de decimal a binarios
sep <- function(punto, frac)
{
  chbase = as.character(punto)
  chfrac = "."
  auxf = 0
  
  while (auxf == 0 | auxf == 1) 
  {
    
    frac = frac * 2
    print(frac)
    auxf = floor(frac)
    if (auxf < 2)
    {
      cad = as.character(auxf)
      chfrac = str_c(chfrac, cad)
      if (frac > 1)
      {
        frac = frac-1
      }
    }

  }
  
  chfract = str_c(chbase, chfrac)
  
  return(chfract)
  
}

#Declaracion de variables
aux1 = pi
numb = as.numeric(aux1)
nume = floor(numb)
frac = numb - nume
print(numb)


#Llamado de funciones
punto = bin(nume)
Resul = sep (punto, frac)
print(Resul)

