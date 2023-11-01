#Librerias 
library(dplyr)
library(ggplot2)
library(readxl)

ENFT2011 = "ENFT_Abril_2011.xlsx"
Vivienda = read_xlsx(ENFT2011, sheet = 'Vivienda')

#1 Quisiera saber si existe una relacion entre las viviendas
#que no tienen calles alfatadas y el acesso al agua publica

#las variables que voy a usar son logicas ambas, Con valor 1 (si) o, 0(no)

#2 C.A. = Calles asfaltadas, A.P. = Agua potable
CA <- Vivienda$EFT_CALLES_ASFALTADAS
AP <- Vivienda$EFT_AGUA_RED_PUBLICA

#Convertir el valor 2 en 1, para obtener las medias mas facilmente
#Sera en 0 
CA[CA == 2] <- 0
AP[AP == 2] <- 0

#Trabajaré con un Dataframe simplificado para mayor facilidad
Data <- data.frame(
  CA,
  AP
)
Data <- na.omit(Data)
fnum <- nrow(Data)

#Covarianza y correlacion
cov(Data$CA, Data$AP)
cor(Data$CA, Data$AP, method = 'pearson')

Totales <- data.frame(
  "Con Ambas" = sum(Data$CA == 1 & Data$AP ==1),
  "Sin Asfalto" =sum(Data$CA == 0),
  "Sin Agua" = sum(Data$CA == 1),
  "Con Asfalto, sin Agua" = sum(Data$CA == 1 & Data$AP ==0),
  "Con Agua, sin Asfalto" = sum(Data$CA == 0 & Data$AP ==1),
  "Sin Ambas" = sum(Data$CA == 0 & Data$AP ==0)
)

#promedios 
Promedios <- data.frame(
  Con_Ambas <- Totales[1]/ fnum * 100,
  Sin_Asfalto <- Totales[2]/ fnum * 100,
  Sin_Agua <- Totales[3]/ fnum * 100,
  Con_Asfalto_sin_Agua <- Totales[4]/ fnum * 100,
  Con_Agua_sin_Asfalto <- Totales[5]/ fnum * 100,
  Sin_Ambas <- Totales[6]/ fnum * 100
)

Vectores <- c( Totales[,1],  Totales[,4], Totales[,5], Totales[,6])
pie(Vectores,
    labels = c('Con Ambas','Con asfalto sin agua','Con Agua sin asfalto','Con ninguna')
)

#El grafico muestra como esta distrribuido el acceso al agua potable y las condiciones
# de las calles entre los hogares.

#Interpretaciones:
#El 42.5% de los hogares tiene accesso a Agua potable y a calles asfaltadas

#El 47.74% de los hogares no tienen asfaltadas las calles a su alrededor

#EL 52.25% de los hogares no tiene acesso al agua potable

#Solo el 9.72% de los hogares que tienen calles asfaltadas a su arrededor no 
#poseen agua potable

#Un 26.85% de los hogares que tienen accesso al Agua potable, 
#no tienen calles asfaltadas en su alrededor

# Increiblemente existe un 20.9% de hogares que no poseen acesso al agua potable 
#ni tiene calles asfaltadas a su alrededor

#los analisis de covarianza y correlacion son ineficientes para esto tipo de 
#analisis, aunque estos muestren alguna relacion, 
#la comparación de las medias los hace mas relevante.


