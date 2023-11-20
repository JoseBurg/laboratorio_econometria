# Paquetes ----------------------------------------------------------------
library(wooldridge)
library(stargazer)
library(dplyr)
library(readxl) # Leer base de datos en excel


miembros <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Miembros")
ocupacion <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Ocupación")
vivienda <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Vivienda")
# Union de bases de datos
datos <- miembros |> 
  left_join(ocupacion,
            by = c("EFT_PERIODO", "EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))


# EJEMPLOS: Estimación de modelos -----------------------------------------
modelo1 <- lm(wage ~ educ, data = wage1) 
modelo2 <- lm(wage ~ educ^2, data = wage1) 
# Formas de ver la estimación del modelo
# 
# 1ra
summary(modelo1)

# 2da
stargazer(modelo1, modelo2, type = "text")

# Estimación de variables de la ENFT --------------------------------------

monto_alquiler <- vivienda$EFT_MONTO_ALQUILER
tiempo_alquiler <- vivienda$EFT_TIEMPO_PAGA_ALQUILER
zona <- vivienda$EFT_ZONA

mod_real1 <- lm(monto_alquiler ~ tiempo_alquiler + zona)


stargazer(mod_real1, type = "text")
