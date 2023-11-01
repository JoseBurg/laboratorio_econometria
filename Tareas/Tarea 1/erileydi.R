#instalar paquetes

install.packages("tidyverse")
install.packages("readxl")
install.packages("datos")
install.packages("dplyr")

#instalar librerias

library("tidyverse")
library("readxl")
library("datos")
library("dplyr")
library(ggplot2)

#buscar la ruta del archivo

file.choose()
ruta1 <-"C:\\Users\\elchi\\Downloads\\ENFT_Abril_2011.xlsx"

#cargar datos a utilizar

miembro <- read_excel(ruta1,sheet = "Miembros")
warnings(miembro)

ocupacion <-read_excel(ruta1,sheet = "Ocupación")
warnings(ocupacion)

#utilizo la funcion warnings solo para visualizar los datos de la hoja

# Ejercicio1. Con la función left_join() unir las hojas “Miembro” y “Ocupación” 
# - de “ENFT_Abril_2011.xlsx”.

datos_unidos  <- left_join(miembro, ocupacion)

# Ejerccio2. Calcular el ingreso promedio (EFT_ING_OCUP_PRINC).

ingresos_promedio <- datos_unidos %>%
  summarize(ingreso_promedio = mean(datos_unidos, na.rm = TRUE)) 
ingresos_promedio <- mean(datos_unidos$EFT_ING_OCUP_PRINC, na.rm = TRUE)
print(ingreso_promedio)

# Ejercicio3. Calcular el ingreso promedio por genero (EFT_SEXO).

ingresos_promedio_genero  <- datos_unidos %>%
  group_by(EFT_SEXO) %>%
  summarise(ingresos_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))
print(ingresos_promedio_genero)

# Ejercicio4. Replicar el gráfico, cambiando la variable ingreso
# por la variable horas trabajadas a la semana (EFT_HORAS_SEM_OCUP_PRINC).

ggplot(datos_unidos, aes(x= EFT_HORAS_SEM_OCUP_PRINC)) +
  geom_density()+
  labs(x="Horas Trabajadas a la Semana", y = "Densidad") +
  ggtitle("Districucion de Horas Trabajadad a la Semana")

horas_trabajadas_semana <- datos_unidos %>%
  summarize(horas_trabajadas_semanas = mean(EFT_HORAS_SEM_OCUP_PRINC, na.rm = TRUE))
print(horas_trabajadas_semana)

# Interpretacion 
# El grafico indica que la tendencia de la variable "datos_unidos" es de
# 40.7 horas de trabajo a la semana.