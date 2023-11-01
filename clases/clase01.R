# Paquetes a usar
library(tidyverse)
library(readxl)

# Crear objetos
jose <- 15
enmanuel <- c(1,5,33,5)


# Funciones
# Help
mean(enmanuel)    # Media o promedio
median(enmanuel)  # mediana 
sd(enmanuel)      # Desviación estándar

# Tipos de datos 
juan <- 10               # Entero
maria <- 27.8            # Numérico 
jordy_compra_pan <- TRUE # Lógicos
clariel <- "Levanto la mano hoy"  # Characters 

# Consultar 
class(maria)


# Transformar datos con la familia de as. 
cuan <- c(5, 10, "l")

# as.numeric()
# as.logical()
# as.integer()

# Operadores aritmeticos
5+5
5*5
5/5
5^5

# Operadores lógico
carlos <- TRUE
casilla <- FALSE

carlos | casilla # Carlos O casilla son TRUE?
carlos & casilla # Carlos Y casilla son TRUE?
!carlos          # Carlos no es verdadero o TRUE?


# Estructura de datos 
# data.frame

notas_econometria <- data.frame(
  nombre = c("juan", "clariel", "jordy"),
  calificacion = c(95,94, 98)
)


# Tidyverse ---------------------------------------------------------------

iris %>% 
  select(Species, Sepal.Length) %>% 
  group_by(Species) %>% 
  summarise(promedio = mean(Sepal.Length))




# Lectura de base de datos
library(readxl)
excel_sheets("./ENFT_Abril_2011.xlsx") # Consultar hojas

encuesta_hogar <- read_excel("./ENFT_Abril_2011.xlsx", sheet = "Hogar")
encuesta_miembros <- read_excel("./ENFT_Abril_2011.xlsx", sheet = "Miembros")

data_completa <- encuesta_hogar %>% 
  left_join(encuesta_miembros, by = c("EFT_PERIODO", "EFT_VIVIENDA", "EFT_HOGAR"))



# Manipulación 

data_completa |> 
  select(EFT_HOGAR, EFT_SEXO, EFT_EDAD) |> 
  filter(EFT_EDAD > 18) |>  # Mayores de edad
  group_by(EFT_SEXO) |> 
  summarise(
    promedio_edad = mean(EFT_EDAD),
    mediana = median(EFT_EDAD)
  )
  

mayores_edad <- data_completa |> 
  select(EFT_HOGAR, EFT_SEXO, EFT_EDAD) |> 
  filter(EFT_EDAD > 18)  # Mayores de edad
  


summary(mayores_edad$EFT_EDAD)


# Graficar

mayores_edad |> 
  ggplot(aes(EFT_EDAD)) + 
  geom_histogram()

mayores_edad |> 
  ggplot(aes(EFT_EDAD,group = EFT_SEXO)) + 
  geom_boxplot()














