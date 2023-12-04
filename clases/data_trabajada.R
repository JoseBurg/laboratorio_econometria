library(readxl)
library(dplyr)

# Hojas disponibles del libro
vivienda <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Vivienda")
hogar <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Hogar")
miembros <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Miembros")
ocupacion <- read_excel("./datos/ENFT_Abril_2011.xlsx", sheet = "Ocupación")


datos <- miembros |> 
  left_join(ocupacion,
            by = c("EFT_PERIODO", "EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))


# Manipulaciones 
datos_manipulados <- datos |>
  filter(!is.na(EFT_ULT_NIVEL_ALCANZADO)) |> 
  mutate(
    sexo = case_when(
      EFT_SEXO == 2 ~ 0, # Mujer
      EFT_SEXO == 1 ~ 1), # Hombre
    # zona = case_when(
    #   EFT_ZONA_RESIDE == 0 ~ "Urbano",
    #   EFT_ZONA_RESIDE == 1 ~ "Rural",
    #   EFT_ZONA_RESIDE == 2 ~ "Extranjero"),
    educacion = case_when(
      EFT_ULT_NIVEL_ALCANZADO == 1 ~ EFT_ULT_ANO_APROBADO,      # Preprimario 
      EFT_ULT_NIVEL_ALCANZADO == 2 ~ EFT_ULT_ANO_APROBADO + 3,  # Primario
      EFT_ULT_NIVEL_ALCANZADO == 3 ~ EFT_ULT_ANO_APROBADO + 11, # Secundario
      EFT_ULT_NIVEL_ALCANZADO == 4 ~ EFT_ULT_ANO_APROBADO + 15, # Vocacional 
      EFT_ULT_NIVEL_ALCANZADO == 5 ~ EFT_ULT_ANO_APROBADO + 19, # Universitario
      EFT_ULT_NIVEL_ALCANZADO == 6 ~ EFT_ULT_ANO_APROBADO + 25, # Postuniversitario
      EFT_ULT_NIVEL_ALCANZADO == 7 ~ EFT_ULT_ANO_APROBADO,      # Cero
    ))

# Nuevo análisis ----------------------------------------------------------

datos_manipulados |> 
  mutate(
    sexo = recode(sexo,
      `0` = "Mujer",
      `1` = "Hombre")) |> 
  ggplot() + 
  geom_histogram(aes(EFT_ING_OCUP_PRINC))+
  facet_wrap(~sexo)
