---
title: "Estimación de MCO"
author: "José Burgos"
date: "2023-10-17"
output:
  beamer_presentation:
    number_sections: false
theme: Madrid
color: whale
---



## Estimaciones de MCO

Modelo econométrico:

$$
y_i = \beta_0 + \beta_1x_i + u_i
$$

### Ecuación:

Variables: $y$ dependiente y $x$ independiente.

Parametros:

$\beta_0$ intercepto o constante.

$\beta_1$ es la pendiente de la recta, es la relación entre $y$ y $x$.

$u$ es el término de error, representa los factores distintos a $x$ que afectan a $y$ .

## Ejemplo de Regresión Lineal Simple


```r
modelo1 <- lm(wage~educ, data = wage1)
summary(modelo1)
```

```
## 
## Call:
## lm(formula = wage ~ educ, data = wage1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.3396 -2.1501 -0.9674  1.1921 16.6085 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.90485    0.68497  -1.321    0.187    
## educ         0.54136    0.05325  10.167   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.378 on 524 degrees of freedom
## Multiple R-squared:  0.1648,	Adjusted R-squared:  0.1632 
## F-statistic: 103.4 on 1 and 524 DF,  p-value: < 2.2e-16
```
