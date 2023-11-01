#Francheska Rosario M. 100577023-Lab_econometria.



# Comentarios -------------------------------------------------------------
# No es necesario guardar el código para instalar los paquetes, esto solo se
# hace una sola vez.


install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)



# Comentarios -------------------------------------------------------------
# Solo cargaste una base de datos, es imposible seguir adelante
##importar los datos
ENFT_Abril_2011<-read_excel("C:/Users/Francheska Rosario/Downloads/ECONOMETRIA/ENFT_Abril_2011.xlsx")


View(ENFT_Abril_2011)
View(ENFT_Abril_ocupacion)
left_join(ENFT_Abril_2011)
?left_join
names(ENFT_Abril_2011)
names(ENFT_Abril_ocupacion)
Comun<-left_join(ENFT_Abril_2011,ENFT_Abril_ocupacion,by="EFT_VIVIENDA" )
view(Comun)



# Comentarios -------------------------------------------------------------
# Aquí duplicaste la operación de sacar el promedio, aunque funcione es un error
# de código

#PTO.2 ingreso_promedio (EFT_ING_OCUP_PRINC).
ingreso_promedio <- mean(ENFT_Abril_ocupacion$EFT_ING_OCUP_PRINC,na.rm = TRUE) # Here
mean(ingreso_promedio) # Here

#PTO.3 Ingreso_promedio (EFT_SEXO)
ingreso_promedio<-mean(ENFT_Abril_2011$EFT_SEXO,na.rm= TRUE) # Esto no esta correcto, estas sacando el promedio de una variable cualitativa.
mean(ingreso_promedio)

#PTO.4 Grafico (EFT_HORAS_SEM_OCUP_PRINC)
ggplot(ENFT_Abril_ocupacion, aes(x=EFT_HORAS_SEM_OCUP_PRINC))+
  geom_density()+
  labs(x="EFT_HORAS_SEM_OCUP_PRINC)", y= "Densidad")+
  ggtitle("Horas trabajadas a la semana")

#explicacion grafico
#En la Encuesta Nacional de Trabajo realizada en abril del año 2011, se observa un fenómeno 
#notable relacionado con la duración de las jornadas laborales de los encuestados. 
#El análisis de los datos revela un elevado auge en las horas trabajadas, 
#específicamente en el rango de 45 a 50 horas trabajadas por semana#. 



