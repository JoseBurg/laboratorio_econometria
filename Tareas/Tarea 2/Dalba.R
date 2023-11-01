#Elegir dos variables de su interés de la ENFT y describirla (Que mide, tipo de variable aleatoria, etc).

## Edad (EFT_EDAD) Es una varible aleatoria continua que representa la edad sujetos
## Ingreso (EFT_ING_OCUP_PRINC) Es una variable aleatoria continua que representa os ingresos de los sujetos

# Aún no estamos en estimación:
## Estamos estimando como se distribuye el salario respecto a la edad de los sujetos

library(tidyverse)
library(readxl)

ENFT_Abril_2011 <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx")

miembros <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx", sheet = "Miembros")
ocupacion <- read_excel("C:/Users/hered/Desktop/Econometria2023/ENFT_Abril_2011.xlsx", sheet = "Ocupación")

database <- ocupacion %>% 
  left_join(miembros, by = c("EFT_PERIODO", "EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))

Data <- database %>% select(EFT_EDAD,EFT_ING_OCUP_PRINC )

Data #data seleccionada

#Calcular valor esperanza, varianza y covariaza

## esperanza

mean(Data$EFT_EDAD) # E(edad)

mean(Data$EFT_ING_OCUP_PRINC, na = TRUE) # E(ingreso)


## varianza

var(Data$EFT_EDAD) #var(edad)

var(Data$EFT_ING_OCUP_PRINC, na = TRUE) #var(ingreso)


## covarianza

Data[is.na(Data)] <- 0

cov(Data$EFT_EDAD, Data$EFT_ING_OCUP_PRINC) ##cov(edad, ingreso)


#Graficos

Data %>% 
  ggplot(aes(x = EFT_EDAD, y = EFT_ING_OCUP_PRINC)) + 
  geom_point()

Data %>% 
  ggplot(aes(x = EFT_EDAD, y = EFT_ING_OCUP_PRINC)) + 
  geom_line()
