#Laboratorio de Econometría 
#Tarea 2
#Rosalvy Nicole Chávez
# 15/ Octubre / 2023


library(tidyverse)
library(readxl)
library(flextable)
library(DescTools)

ENFT_Abril_2011 <- read_excel("./ENFT_Abril_2011.xlsx")

MIEMBROS <- read_excel("./ENFT_Abril_2011.xlsx", sheet = "Miembros")
OCUPACION <- read_excel("./ENFT_Abril_2011.xlsx", sheet = "Ocupación")

# Vamos a combinar los datos por estas columnas en especifico, así evitamos posibles errores
Data_Combinada <- left_join(MIEMBROS, OCUPACION, by = c("EFT_PERIODO","EFT_VIVIENDA", "EFT_HOGAR", "EFT_MIEMBRO"))


#Punto 1. Elegir dos variables de su interés de la ENFT y describirla
#(Que mide, tipo de variable aleatoria, etc).

## Las variables seleccionadas son:

##Nivel de Ingreso: es una variable aleatoria continua que recoge el
## salario de los miembros de la muestra estudiada.

## Edad: es una variable aleatoria discreta que recoge las edades
## de los miembros de la muestra estudiada.  


Nueva_Data <- Data_Combinada %>%
  mutate(Ingreso = EFT_ING_OCUP_PRINC,
         Edad = EFT_EDAD)


datos_trabajados <- 
Nueva_Data %>%
  filter(!is.na(Edad)) %>%
  # Calcularlo por cada año no es eficiente
  # Es mejor por rango de edad, como:
  mutate(
    Edad = case_when(
      Edad <= 13 ~ "Niñez",
      Edad >= 14  & Edad <= 17 ~ "Adolescencia",
      Edad >= 18  & Edad <= 35 ~ "Adultos jovenes",
      Edad >= 36  & Edad <= 64 ~ "Adultos",
      Edad >= 65 ~ "Tercera edad"))

datos_trabajados |> 
  group_by(Edad) %>%
  summarise(
    Promedio_Ingreso = mean(Ingreso, na.rm = TRUE),
    Mediana_Ingreso = median(Ingreso, na.rm = TRUE),
    Máximo_Ingreso = max(Ingreso, na.rm = TRUE),
    Mínimo_Ingreso = min(Ingreso, na.rm = TRUE)) %>%
  mutate(Edad = factor(Edad, labels = c(
           "Niñez", "Adolescencia",
           "Adultos jovenes", "Adultos", "Tercera edad"))) |> 
  janitor::clean_names(case = "title") |>   # Con esta función conviertes
  arrange(Edad) |>                          # Las col en formatos de titulos
  flextable() %>%                           
  autofit() %>%
  set_caption("Medidas del Ingreso por Edad")


datos_trabajados %>%
  select(Ingreso, Edad) %>%
  group_by(Edad) %>%
  summarize(Promedio= mean(Ingreso, na.rm = TRUE)) %>%
  flextable() %>%
  autofit() %>%
  set_caption("Ingreso Promedio Por Edad")

# Promedio_Ing_Edad <- Nueva_Data %>% # Solo asigna si lo vas a usar en otro lugar
  
datos_trabajados |> 
  select(Ingreso, Edad) %>%
  group_by(Edad) %>%
  summarize(Promedio_Ing = mean(Ingreso, na.rm = TRUE))

#Punto 2. Calcular las siguientes medidas:E(X), Var(X), Cov(X,Y).

Variable_X <- Nueva_Data %>%
  select(Edad)

Variable_Y <- Nueva_Data %>%
  select(Ingreso)

Valor_EsperadoX <- Variable_X %>%
  summarise(mean(Variable_X$Edad))

VarianzaX <- Variable_X %>%
  var(Variable_X$Edad, na.rm = TRUE) 

Covarianza_XY <- cov(Variable_X, Variable_Y, use= "complete.obs")



#Punto 3. Realizar gráficos con diferentes geometrías que permitan 
#conocer más de sus variables, por ejemplo: geom_line(), geom_boxplot(). 
#Para más gráficos consultar: The R Graph Gallery.

Nueva_Data %>%
  ggplot(aes(Edad)) +
  geom_histogram(fill= "lightblue", color= "blue") +
  labs(title = "Histograma de Edad", 
       caption = "Datos ENFT_Abril_2011", 
       x= "Edad",
       y= "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))


Nueva_Data %>%
  ggplot(aes(Edad, Ingreso)) +
  geom_col() +                                      # Así se ve mejor 
  labs(title = "Grafico Nivel de Ingreso por Edad", 
       caption = "Datos ENFT_Abril_2011", 
       x= "Edad") +
  theme(plot.title = element_text(hjust = 0.5))


Nueva_Data %>%
  ggplot(aes(Edad)) +
  geom_density(fill= "lightgreen", color= "green") +
  labs(title = "Grafico De Densidad por Edad", 
       caption = "Datos ENFT_Abril_2011", 
       x= "Edad") +
  theme(plot.title = element_text(hjust = 0.5))


Nueva_Data %>%
  ggplot(aes(Edad, Ingreso)) +
  geom_point() +
  labs(title = "Grafico Nivel de Ingreso por Edad", 
       caption = "Datos ENFT_Abril_2011", 
       x= "Edad") +
  theme(plot.title = element_text(hjust = 0.5))



Promedio_Ing_Edad %>%
  ggplot(aes(Promedio_Ing)) +
  geom_density(fill= "violet", color= "purple") +
  labs(title = "Grafico De Densidad Promedio Ingreso por Edad", 
       caption = "Datos ENFT_Abril_2011", 
       x= "Promedio Ingreso por Edad") +
  theme(plot.title = element_text(hjust = 0.5))
