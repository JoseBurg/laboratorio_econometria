library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

Miembros <- read_excel("ENFT_Abril_2011.xlsx",sheet = "Miembros")
ocupacion <- read_excel("ENFT_Abril_2011.xlsx",sheet = "Ocupación")

#1. Elegir dos variables de su interés de la ENFT y describirla

#las variables que me interesan analizar son el ingreso y el nivel académico alcanzado.
#Mi objetivo es descubrir si existe una dependencia del ingreso respecto al nivel academico del individuo. 

datos<- Miembros|>
  left_join(ocupacion,
            by=c("EFT_PERIODO","EFT_VIVIENDA","EFT_HOGAR","EFT_MIEMBRO")) 

datos_manipulados <- datos |> 
  mutate(
    Educacion=case_when(
      EFT_ULT_NIVEL_ALCANZADO==1~"PREPRIMARIO",
      EFT_ULT_NIVEL_ALCANZADO==2~"PRIMARIO",
      EFT_ULT_NIVEL_ALCANZADO==3~"SECUNDARIO",
      EFT_ULT_NIVEL_ALCANZADO==4~"VOCACIONAL",
      EFT_ULT_NIVEL_ALCANZADO==5~"UNIVERSITARIO",
      EFT_ULT_NIVEL_ALCANZADO==6~"POSTUNIVERSITARIO",
      EFT_ULT_NIVEL_ALCANZADO==7~"NINGUNO"),
    Educacion = factor(Educacion, level = c("NINGUNO",
                                             "PREPRIMARIO",
                                             "PRIMARIO",
                                             "SECUNDARIO",
                                             "VOCACIONAL",
                                             "UNIVERSITARIO",
                                             "POSTUNIVERSITARIO"))
    ) |> filter(!is.na(Educacion)) # Para escluir los NA

datos_manipulados |> 
  select(Educacion,EFT_ING_OCUP_PRINC) |> 
  group_by(Educacion,) |> 
  summarise(promedio_Ing = mean(EFT_ING_OCUP_PRINC,na.rm = TRUE),
            Varianza_Ing = var(EFT_ING_OCUP_PRINC,na.rm = TRUE))
#para mejor visualización:
attach(datos_manipulados) # Esto no es necesario
ing_pro_X_NL_Acad = aggregate(EFT_ING_OCUP_PRINC ~ datos_manipulados$Educacion,data = datos_manipulados  , FUN = mean) # Tampoco esto

#interpretación:
#me doy cuenta que existe una proporcionalidad directo entre el nivel académico y el promedio de ingreso de los individuos.
#A mayor nivel académico, mayor es el promedio de ingresos.



#Realizar gráficos con diferentes geometrías que permitan conocer más de sus variables, por ejemplo: geom_line(), geom_boxplot().


Grafica_lineal<-ggplot(data=datos_manipulados)+
  geom_line(aes(x=datos_manipulados$Educacion, y=EFT_ING_OCUP_PRINC))
Grafica_lineal+
  labs(title = "Gráfico del nivel de ingreso segun su nivel academido",
       subtitle = "Datos de ENFT_Abril_2011",
       caption = "Jordy De La Paz /2023",
       x = "nivel academico",
       y = "ingreso")+
  scale_y_continuous(breaks = seq(10000,300000,by=50000))
#tengo dificultad para generar el grafico, aca necesito ayuda. 

texto <- labs(title = "Gráfico del nivel de ingreso segun su nivel academido",
                subtitle = "Datos de ENFT_Abril_2011",
                caption = "Jordy De La Paz /2023",
                x = "nivel academico",
                y = "ingreso")+
  scale_y_continuous(breaks = seq(10000,300000,by=50000))

# Simple:

datos_manipulados |> 
  ggplot() +
  geom_boxplot(aes(Educacion, EFT_ING_OCUP_PRINC)) +
  labs(
    title = "Gráfico del nivel de ingreso segun su nivel academido",
    subtitle = "Datos de ENFT_Abril_2011",
    caption = "Jordy De La Paz /2023",
    x = "nivel academico",
    y = "ingreso") +
  scale_y_continuous(breaks = seq(10000,300000,by=50000))
