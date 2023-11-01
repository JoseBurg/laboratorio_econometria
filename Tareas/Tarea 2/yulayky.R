#yulayky maldonado 100465682

library(tidyverse)
library(readxl)
library(stats)
library(ggplot2)

#importar datos
# Esto no es necesario:
archivo_excel <- read_excel("C:/Users/DELL/Desktop/Lab Econometria/ENFT_Abril_2011.xlsx")

#ubicacion archivo
#Si esto es lo unico que vas a usar:
ubicacion_archivo <- "C:/Users/DELL/Desktop/Lab Econometria/ENFT_Abril_2011.xlsx"


#lasvariables escigidas son remesas y ocupacion       # Segura?
#leer hojas del excel remesas y ocupacion
x <- read_excel(ubicacion_archivo, sheet = "Miembros") # Código duplicado
y <- read_excel(ubicacion_archivo, sheet = "Miembros")

media <- mean(x$EFT_EDAD)
print(media)
varianza  <- var(x$EFT_EDAD, na.rm = TRUE )
print(varianza)
covarianza <- cov(x$EFT_EDAD, y$EFT_PARENTESCO_CON_JEFE)
print(covarianza)


#MEdia
ggplot(x, aes(x = 1, y = EFT_EDAD)) +
  geom_line() +
  labs(title = "Media de EFT_EDAD", x = NULL, y = "Valor Medio") +
  theme_minimal()

# varianza 
ggplot(x, aes(x = NULL, y = EFT_EDAD)) +
  geom_boxplot() +
  labs(title = "Varianza de EFT_EDAD", x = NULL, y = "Varianza") +
  theme_minimal()

# covarianza 
ggplot(y, aes(x = EFT_PARENTESCO_CON_JEFE, y = x$EFT_EDAD)) +
  geom_point() +
  labs(title = "Covarianza entre EFT_EDAD y EFT_PARENTESCO_CON_JEFE", 
       x = "EFT_PARENTESCO_CON_JEFE", y = "EFT_EDAD") +
  theme_minimal()


#Analisis : la edad promedio es 29. 7, la varianza es de 448.7
#esto significa que la edad variacion de las edades es muy alta
#la covarianza es -17.3 significa que cuando la edad aumenta la relacion
#con el jefe disminuye, segun el diccionario mientras mas menor es valor de esa 
#variable representan que la relacion con el jefe es mas directa, siendo 
#esposa, hijo o jefe. 
