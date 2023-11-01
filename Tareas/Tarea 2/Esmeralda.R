# Coloca aquí tu nombre y matricula para la próxima



#1.Elegir dos variables de su interés de la ENFT y describirla 
#(Que mide, tipo de variable aleatoria, etc).

library(tidyverse)
library(readxl)

library(dplyr) # Es necesario cargar los paquetes que utilizaras --------------------

# Recuerda usar la lógica, la siguiente linea no era necesaria: --------------------
# ENFT_Abril_2011 <- read_excel("C:/Users/privado/Downloads/ENFT_Abril_2011.xlsx") 

# Esta si, es la que utilizas:
Vivienda <- read_excel("C:/Users/privado/Downloads/ENFT_Abril_2011.xlsx",
                       sheet = "Vivienda")
attach(Vivienda) # Esto no es necesario --------------------------------------------

#Nota: Solo utilice una pagina de la base de datos. 

###Variables elegidas:
#EFT_ESTADO_VIVIENDA, en la encuesta es una variable aleatoria discreta, ya que en la base de datos 
#solo toma valores enteros, y mide (1) si la case No necesita reparaciones,
# (2) si necesita reparaciones pequeñas o (3) necesita reparaciones importantes/ (Variable cualitativa)

#EFT_MONTO_ALQUILER, variable aleatoria continua, toma valores dentro de un rango determinado, 
#mide la cantidad monetaria que pagan los encuestados por aquiler. 

#2 Calcular las siguientes medidas: Media, Varianza y Covarianza.

# Excelente, me gusto esto!
Resultados <- Vivienda %>%
  mutate(Reparaciones = case_when( 
    EFT_ESTADO_VIVIENDA == 1 ~ "No Reparaciones",
    EFT_ESTADO_VIVIENDA == 2 ~ "Pequeñas Reparaciones",
    EFT_ESTADO_VIVIENDA == 3 ~ "Grandes Reparaciones"))


attach(Resultados) # Esta de más -----------------------------------------------

Resultados %>%
  group_by(Reparaciones) %>%
  summarise(
    Promedio_Alquiler= mean(EFT_MONTO_ALQUILER, na.rm = TRUE),
    Varianza_Alquiler= var(EFT_MONTO_ALQUILER, na.rm = TRUE)
  )

#Nota: el codigo de la covarianza seria algo asi Cov_Alquiler_Estado = cov(EFT_MONTO_ALQUILER, EFT_ESTADO_VIVIENDA), 
#pero esto me da NA, ya que tengo una variable cuantitativa y una cualitativa, por eso no la inclui. Mas adelante practicare con 
#dos variables cuantitativas y se lo enviare. 


#3. Realizar gráficos con diferentes geometrías que permitan conocer más de sus variables, por ejemplo:
#geom_line(), geom_boxplot().

Resultados %>%
  ggplot()+
  geom_line(aes(Reparaciones, EFT_MONTO_ALQUILER))

# Con esto se ve mejor
Resultados %>%
  ggplot()+
  geom_boxplot(aes(Reparaciones, EFT_MONTO_ALQUILER))

#Interpretacion: en este grafico simple se puede observar que el la linea mas alta indica que 
#las casas con mayor monto de alquiler son aquellas que no necesitan reparaciones, y todo lo contrario
#con la linea mas pequeña, menor monto, más reparaciones. 



