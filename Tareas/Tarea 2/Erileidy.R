#Erileidy Roa 100406142
#Tarea 2 laboratorio econometria

library("tidyverse")
library("readxl")
library("dplyr")
library("ggplot2")

#variables (hogar y calculadas)
#Estas son variables aleatorias discretas que miden la ingresos totales de las 
#viviendas por tipo de ingresos y las condiciones de los miembros por vivienda.

file.choose()
data_base1<-"C:\\Users\\elchi\\Downloads\\ENFT_Abril_2011.xlsx" # Tu ruta es tan larga debido a no trabajar en un proyecto. O puede ser que no le estas sacando el mayor provecho.

hogar <- read_excel(data_base1,sheet = "Hogar")
calculadas <-read_excel(data_base1,sheet = "Calculadas")
data_base <- left_join(hogar, calculadas)


ingresos_mensuales_miembros <- data_base %>%     # Esto esta mal
  select(-EFT_MIEMBRO,-EFT_INGRESO_MENSUAL) %>%  # Estas excluyendo las variables que deberias seleccionar con el "-" delante de la variable
  head(n=15)

ingresos_mensuales_vivienda <- data_base %>%
  select(-EFT_VIVIENDA,-EFT_INGRESO_MENSUAL) %>%
  head(n=15) # Si utilizas esta función como asignación limitaras la base a la cantidad de observaciones que estas consultando

#valor esperado  
valor_esperado_data_base <- mean(ingresos_mensuales_miembros$EFT_VIVIENDA) # Esta es una variable cualitativa, aplicarle el promedio es un grave error.

#grafico que muestra los ingresos mensuales por vivienda
ggplot(data_base, aes(x= EFT_VIVIENDA)) +
  geom_density()+
  labs(x="ingresos_mensuales_miembros", y = "Densidad") +
  ggtitle("valor esperado ingresos de los miembros por vivienda")

#varianza
varianza_ingresos_mensuales_miembros <- var(ingresos_mensuales_miembros) # Aplicarle la varianza a la base de datos no tiene sentido

#covarianza 
X <- data_base$EFT_INGRESO_MENSUAL
Y <- data_base$EFT_VIVIENDA
covariaza_XY <- cov(X,Y)

# Falto la interpretación y la definición de las variables, esto es lo más importante.


