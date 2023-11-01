library(readxl)
library(tidyverse)

Data <- read_excel("ENFT_Abril_2011.xlsx")
Data.Miembro <- read_excel("ENFT_Abril_2011.xlsx", sheet = "Miembros")
Data.Occu <- read_excel("ENFT_Abril_2011.xlsx", sheet = "Ocupación")

#1 Unir Miembros y ocupacion usando left_Join()
Data.Miembro.Occu <- left_join(Data.Miembro, Data.Occu)

#2 Ingreso promedio
Data.Miembro.Occu %>%
  summarise(mean = mean(EFT_ING_OCUP_PRINC, na.rm=TRUE))

#3ingreso promedio por sexo
Data.Miembro.Occu%>%
  group_by(EFT_SEXO)%>%
  summarise(Promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

#4 Grafico
Data.Miembro.Occu %>%
  ggplot(aes(x = EFT_HORAS_SEM_OCUP_PRINC)) + geom_density()

# #Interpretacion: el grafico refleja que por su forma se asemeja a una frecuencia de distribucion normal
# aunque pudiese se pura casualidad, basicamente esta mostrando que la mayoria de personas labora en su ocupacion principal
# entre 37.5 y 53 horas a la semana, correspondiendo en parte a la ley 16-92 que establece las horas de trabajo ordinarias en 44. 
