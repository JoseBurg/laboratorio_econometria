###Esmeralda Morillo 100532475###

#Laboratorio Econometria-Tarea 1#

#Manipulacion de datos utilizando readxl y tidyverse#

#1.Con  la  función  left_join()  unir  las  hojas  “Miembro”  y  “Ocupación”  de  “ENFT_Abril_2011.xlsx”.#

library(tidyverse)
library(readxl)


ENFT_Abril_2011 <- read_excel("C:/Users/privado/Downloads/ENFT_Abril_2011.xlsx")

Ocupacion <- read_excel("C:/Users/privado/Downloads/ENFT_Abril_2011.xlsx",
                        sheet = "Ocupacion") # Esto no me corre, el nombre debe de estar bien escrito 

Miembros <- read_excel ("C:/Users/privado/Downloads/ENFT_Abril_2011.xlsx",
                        sheet = "Miembros")

ENFT_MO <- Ocupacion %>%
  left_join(Miembros, by= c("EFT_PERIODO","EFT_VIVIENDA", "EFT_HOGAR","EFT_SEXO")) # Esto no funciona, de las variables que colocas en by deben estar en las dos bases de datos y deben de tener lógica
# 

# Así:
ENFT_MO <- Ocupacion %>%
  left_join(Miembros, by= c("EFT_PERIODO","EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))

#2.   Calcular  el  ingreso  promedio  (EFT_ING_OCUP_PRINC).

ENFT_MO %>%
  summarise(Ingreso_Promedio=
               mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

#3.Calcular  el  ingreso  promedio  por  genero  (EFT_SEXO).

ENFT_MO%>%
  group_by(EFT_SEXO) %>%
  summarise(Promedio_Genero=mean(EFT_ING_OCUP_PRINC, na.rm = TRUE
  ))

#Replicar  el  siguiente  gráfico,  cambiando  la  variable  ingreso  por  la  variable  
#horas  trabajadas  a  la  semana (EFT_HORAS_SEM_OCUP_PRINC),  diga  que  entiende  con  el  resultado.

ENFT_MO %>%
  ggplot(aes(x= EFT_HORAS_SEM_OCUP_PRINC)) +
  geom_density(color= " royal blue", size= 2, fill= "blue")

##Analisis del Grafico:
#En el eje "x" tenemos la variable que representa el numero de horas semanales que registran los 
#encuestados en su ocupacion principal, mientras que el eje "y" se puede observar la densidad o frecuencia
#relativa. El pico mas alto es alcanzado en 48-49 horas, donde se registra un 0.06 de densidad, lo que 
#demuestra que la mayoria de encuestados dedican de 48-49 horas a la semana en su trabajo principal. 
