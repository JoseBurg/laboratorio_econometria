#paquetes

install.packages("tidyverse")
install.packages("readxl")
install.packages("datos")
install.packages("dplyr")

#librerias

library("tidyverse")
library("readxl")
library("datos") 
library("dplyr") 
library(ggplot2) 

#ruta de archivo

file.choose()
data_base<-"C:\\Users\\arlyn\\Downloads\\ENFT_Abril_2011.xlsx"

#cargar datos 

miembro <- read_excel(data_base,sheet = "Miembros")

ocupacion <-read_excel(data_base,sheet = "Ocupación")


# Ejercicio 1. Con la función left_join() unir las hojas “Miembro” y “Ocupación” 
# - de “ENFT_Abril_2011.xlsx”.

data_base <- left_join(miembro, ocupacion)

# Ejerccio 2. Calcular el ingreso promedio (EFT_ING_OCUP_PRINC).

ingresos_promedio <- data_base %>%
  ingresos_promedio <- mean(data_base$EFT_ING_OCUP_PRINC, na.rm = TRUE)
print(ingreso_promedio)

# Ejercicio3. Calcular el ingreso promedio por genero (EFT_SEXO).

ingresos_promedio_genero  <- data_base %>%
  group_by(EFT_SEXO) %>%
  summarise(ingresos_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))
print(ingresos_promedio_genero)

# Ejercicio 4. Replicar el gráfico, cambiando la variable ingreso
# por la variable horas trabajadas a la semana (EFT_HORAS_SEM_OCUP_PRINC).

ggplot(data_base, aes(x= EFT_HORAS_SEM_OCUP_PRINC)) +
  geom_density()+
  labs(x="Horas Trabajadas a la Semana", y = "Densidad") +
  ggtitle("Districucion de Horas Trabajadad a la Semana")

horas_trabajadas_semana <- data_base %>%
  summarize(horas_trabajadas_semanas = mean(EFT_HORAS_SEM_OCUP_PRINC, na.rm = 
                                              TRUE))
print(horas_trabajadas_semana)

# Interpretacion 
# los miembros en promedio trabajan 40.7 horas a la semana