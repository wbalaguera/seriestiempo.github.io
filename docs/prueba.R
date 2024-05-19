library(readxl)
petroleo1 <- read_excel("D:/Descargas/petroleo1.xls")
View(petroleo1)
attach(petroleo1)
names(petroleo1)
petroleo=petroleo1

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(forecast)
library(ggfortify)
library(tseries)

petroleo.ts=ts(petroleo,start=2013,frequency=12)
petroleo.ts
plot(petroleo.ts)
#serie de tiempo por medio de la funcion ts
precio.ts = ts(petroleo.ts[,1], start =2013 ,freq=12)
plot(precio.ts)
class(precio.ts)
# se realiza un analisis de grafico para identificar el
#nuevo comportamiento
plot (decompose(precio.ts))


##con la funcion autoplot realizamos un analisis
#para identificar la estacionalidad del modelo
autoplot(precio.ts,ts.colour="blue",ts.linetype="dashed")

#realizamos la autocorelaccion y la autocovarianza
#para validad si tenemos una seie estacionaria

# la grafica no hayuna covarianza constante se concluye que noe s estacionaria no hay caidas exponenciales

autoplot(acf(precio.ts, type="correlation",plot=FALSE))
acf(precio.ts,type="covariance",plot=TRUE)


library(forecast)
#difrenciacion regulares
ndiffs (precio.ts)
#a lo que quiere decir se le debemos aplicar uan difreciacion regular

nsdiffs (precio.ts)

#realizmo s serie e tiempo con difrenciacion
diff.precio.ts<- autoplot(diff(precio.ts),ts.colour="red")
diff.precio.ts
# lo que quiere decir es que mejora la estacionalaidad por
#aumenta disminuye



#RAICES UNITARIAS
# H0 LA SERIE TIENE RAICES UNITARIAS (NO ESTACIONARIA)
# H1 LA SERIE TIENE RAICES UNITARIAS ( ESTACIONARIA)

#PVALUE MENOR A 0.05  RECHLAZA LA HIPOTESIS NULA
#PVALUE MAYOR A 0.05  APRUEBA LA HIPOTESIS NULA  OSEA QUE NO ES ESTACIONARIA

adf.test(precio.ts)#0.73
#la serie es mayor a 0.05 por lo cual  no es estacionaria



#para soluciona r este problema vinculamos las fdifernes difrenciaciones
adf.test(diff(precio.ts))#= 0.07162
#la serie es mayor a 0.05 por lo cual  no es estacionaria


#realizanos segunda difrenciacion
adf.test(diff(diff(precio.ts)))
#la serie es menorr a 0.05 por lo cual  es estacionaria

#en caso de que no hubiera dado
adf.test(diff(log(precio.ts))) # si ese hace cone ste algotimo se pasa a un numero natural

#



# CREACION DEL MODELO ARIMA
#c(0,1,0) 0 regresiion 1 difrenciacion  y 0 media vil
arima1<- Arima(precio.ts,order =c(0,1,0) ,seasonal =list(order=c(0,0,0),period=12) )
arima1a<- Arima(precio.ts,order =c(1,1,0) ,seasonal =list(order=c(0,0,0),period=12) )
arima2<- Arima(precio.ts,order =c(0,1,0) ,seasonal =list(order=c(1,0,0),period=12) )
arima3<- Arima(precio.ts,order =c(1,0,0) ,seasonal =list(order=c(0,0,1),period=12) )
arima4<- Arima(precio.ts,order =c(2,2,1) ,seasonal =list(order=c(0,0,0),period=12) )
arima5<- Arima(precio.ts,order =c(0,1,1) ,seasonal =list(order=c(0,0,0),period=12) )
arima6<- Arima(precio.ts,order =c(2,2,0) ,seasonal =list(order=c(0,0,0),period=12) )
arima7<- Arima(precio.ts,order =c(2,1,0) ,seasonal =list(order=c(0,0,0),period=12) )
arima8<- Arima(precio.ts,order =c(3,2,2) ,seasonal =list(order=c(0,0,0),period=12) )



AIC(arima1,arima1a,arima2,arima3,arima4,arima5,arima6,arima7,arima8)
# EL MEJOR MODELO CON AIC ES EL MODELO 1a YA QUE TIENE EL MODELO MAS BAJO
BIC(arima1,arima1a,arima2,arima3,arima4,arima5,arima6,arima7,arima8)

auto.arima(precio.ts)
# EL MEJOR MODELO CON BIC ES EL MODELO 1a YA QUE TIENE EL MODELO MAS BAJO


#DE LOS MODELOS QUE HEMOS CREADO GRAFICAMOS LOS RESIDUOS
autoplot(acf(arima1a$residuals,plot=FALSE))
# varianza de los errores
autoplot(pacf(arima1a$residuals,plot=FALSE))

#analisis de los residuos de manera individual
ggtsdiag(arima1a)
summary(arima1a)


#pronostico del mdelo
#95 porcient de confianza h=12 es la feq
pronostico<- forecast(arima1a,level=c(95),h=12)
plot(pronostico)
pronostico




