#Declaracion de paquetes y librerias usadas
install.packages("tidyverse")
library(tidyverse)
library(stringr)


#Declaracion de Variables
cadena = readline("Numero: ")
ncad = chartr(".","/",cadena)
separa = strsplit(ncad, "/")
vecto = unlist(separa, use.names=FALSE)
entero = vecto [1:1]
tament = nchar(entero)
frac = vecto [2:2]
if (is.na(frac)){
  frac = "0"
  tamfrac = 0
}else {
  tamfrac = nchar(frac)
}


#Funcion para pasar de binario a decimal
deci <- function (entero, frac, tament, tamfrac)
{
  
  #Parte entera del Binario
  cont1 = tament
  cont2 = tamfrac
  cont3 = -1
  total = 0
  expo = 0
  
  while (cont1 > 0){
    
    p = str_sub(entero, cont3,cont3)
    n = as.numeric(p)
    total = total + (n)*2^expo
    expo = expo + 1
    cont1 = cont1 - 1
    cont3 = cont3 - 1
    
  }
  
  
  #Parte fraccional
  cont3 = 1
  expo = -1
  
  while(cont2 > 0){
    
    q = str_sub(frac, cont3,cont3)
    m = as.numeric(q)
    total = total + (m)*2^expo
    expo = expo - 1
    cont2 = cont2 - 1
    cont3 = cont3 + 1
  }
  
  return(total)
  
}

#Llamado a funcion
resultado = deci (entero, frac, tament, tamfrac)
print(resultado)
