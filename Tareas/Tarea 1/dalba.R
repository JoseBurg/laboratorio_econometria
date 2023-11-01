# Tarea de Laboratorio de Econometria 1

library(tidyverse)
library(readxl)

## 1. Con la función left_join() unir las hojas "Miembro" y "Ocupación" de "ENFT_Abril_2011.xlsx".

ENFT_Abril_2011 <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx")

excel_sheets("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx") 

miembros <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx", sheet = "Miembros")
ocupacion <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx", sheet = "Ocupación")

database <- ocupacion %>% 
  left_join(miembros, by = c("EFT_PERIODO", "EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))


## 2. Calcular el ingreso promedio (EFT_ING_OCUP_PRINC).

ingreso_promedio <- mean(database$EFT_ING_OCUP_PRINC, na.rm= TRUE)
ingreso_promedio #Resultado

## 3. Calcular el ingreso promedio por genero (EFT_SEXO). (1=hombre, 2=mujer)

ingreso_basado_sexo <-
  database %>%
  select (EFT_SEXO, EFT_ING_OCUP_PRINC, EFT_HORAS_SEM_OCUP_PRINC ) %>%
  group_by(EFT_SEXO) %>%
  summarise(ingreso_promedio_sexo = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))
ingreso_basado_sexo #Resultado


## 4. Replicar  gráfico, cambiando la variable ingreso por la variable horas trabajadas a la semana (EFT_HORAS_SEM_OCUP_PRINC), diga que entiende con el resultado.

database %>% 
  ggplot() + 
  geom_density(aes(EFT_HORAS_SEM_OCUP_PRINC))
#La mayor parte de la poblacion trabaja de entre 40 y 50 horas semanales 
