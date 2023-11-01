install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(readxl)

file.choose() 
datos1 <-"/Users/valerinejimenez/Downloads/ENFT_Abril_2011.xlsx"

#Ejercicio1: Elegir dos variables de su interés de la ENFT y describirla 
#(Que mide, tipo de variable aleatoria, etc).

#Variables a utilizar
tasasmonedas <-read_excel(datos1, sheet = "Tasas monedas")
remesas <-read_excel(datos1, sheet = "Remesas")

RemesasFuncionDolar <- left_join(remesas, tasasmonedas)

#la variable tasas monedas se refiere a La tasa o tipo de cambio entre dos 
#divisas es la tasa o relación de proporción que existe entre el valor de una
#y la otra

#la variable remesas es, básicamente, una transferencia internacional de dinero, 
#que se realiza de un remitente a un destinatario entre diferentes países.

#Ejercicio2: Calcular las siguientes medidas: E(X), Var(X), Cov(X, Y )

#vector: Recibo de remesas en funcion de la tasa del dolar
remesasDolar <- RemesasFuncionDolar %>% 
  left_join(remesas, by = c("EFT_RECIBIO_REMESA", "EFT_TASA_A_DOLAR"))


tasadolar <- c(37.48, 1.00, 1.00, 0.73)
recibioremesas <- c(2.00, 2.00, 2.00, 2.00, 2.00)

#calcular E(X)
EsperanzaXtasadolar <- mean(tasadolar)
EsperanzaXrecibioremesas <- mean(recibioremesas)

#calcular Var(x)
VarianzaXtasadolar <- var(tasadolar)
Varianzaxrecibioremesas <- var(recibioremesas)

#calcular Cov(x, y)
x <- RemesasFuncionDolar$EFT_TASA_A_DOLAR
y <- RemesasFuncionDolar$EFT_RECIBIO_REMESA

Covarianza_tD_RR <- cov(x,y)

#EJERCICIO3: Realizar gráficos con diferentes geometrías
#que permitan conocer más de sus variables, por ejemplo:
#geom_line(), geom_boxplot(). Para más gráficos consultar: The R Graph Gallery

ggplot(RemesasFuncionDolar, aes(x= EFT_RECIBIO_REMESA)) + geom_density()+
  labs("remesasDolar", y= "tasadolar") + 
  ggtitle("Recibo de remesas en funcion a la tasa del dolar")
