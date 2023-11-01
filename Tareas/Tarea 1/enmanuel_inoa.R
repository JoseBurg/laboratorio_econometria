library(readxl)
library(tidyverse)


base_de_datos_excel <-"/Users/enmanuelinoagonzalez/Downloads/ENFT_Abril_2011.xlsx"
base_de_datos_excel <-"./ENFT_Abril_2011.xlsx"

excel_sheets(base_de_datos_excel)

Miembro <- read_excel (base_de_datos_excel, sheet = "Miembros")
ocupación <- read_excel (base_de_datos_excel, sheet = "Ocupación")
Base_unida <- left_join (Miembro, ocupación)

Ingreso_promedio <- Base_unida %>%
  summarise(ingreso_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

Ingreso_genero <- Base_unida %>%
  group_by (EFT_SEXO) %>%
  summarise(ingreso_promedio = mean(EFT_ING_OCUP_PRINC, na.rm = TRUE))

install.packages (ggplot2)

library(ggplot2)

Base_unida %>%
  ggplot(aes(x = EFT_HORAS_SEM_OCUP_PRINC)) +
  geom_density()
#respuesta al gráfico: segun nos muestra el gráfico de densidad, vemos que
#la mayor agrupación de los datos se encuentra entre 48 y 50 horas de trabajo aproximadamente.
#mostrando que la mayor parte de los encuenstrado trabajan esas cantidad de horas.


