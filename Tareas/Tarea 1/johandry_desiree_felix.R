library(tidyverse)
library(readxl)

ubicacion_archivo <- "C:/Users/PC/Downloads/ENFT_Abril_2011.xlsx"


miembros <- read_excel(ubicacion_archivo, sheet = "Miembros")
ocupacion <- read_excel(ubicacion_archivo, sheet = "Ocupación")

columnas_en_comun <- intersect(names(miembros), names(ocupacion))
# print(columnas_en_comun)
columnas_en_comun # No es necesario el print, puedes usar solo el nombre
                  # De la variable

# Aunque funcione, es recomendable hacerlos por estas cuatros variables
# Así evitamos posibles errores de consistencia.
datos_completos <- left_join(miembros, ocupacion, 
                             by = c("EFT_PERIODO", "EFT_VIVIENDA",
                                    "EFT_HOGAR", "EFT_MIEMBRO"))

ingreso_promedio <- datos_completos %>%
  summarize(ingreso_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

# print(ingreso_promedio)
ingreso_promedio

ingreso_promedio_por_genero <- datos_completos %>%
  group_by(EFT_SEXO) %>%
  summarize(ingreso_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

# print(ingreso_promedio_por_genero)
ingreso_promedio_por_genero

ggplot(datos_completos, aes(x = EFT_HORAS_SEM_OCUP_PRINC)) +
  geom_density() +
  labs(x = "Horas Trabajadas a la Semana", y = "Densidad") +
  ggtitle("Distribución de Horas Trabajadas a la Semana")


horas_trabajadas_a_semanas <- datos_completos %>%
  summarize(horas_trabajadas_a_semanas = mean(EFT_HORAS_SEM_OCUP_PRINC, na.rm = TRUE))

# print(horas_trabajadas_a_semanas)

horas_trabajadas_a_semanas

