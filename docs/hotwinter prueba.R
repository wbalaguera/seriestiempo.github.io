library(TSA)


data(airmiles)
View(airmiles)
plot(airmiles)

str(airmiles)
class(airmiles)
start(airmiles)
end(airmiles)
frequency(airmiles)
summary(airmiles)

##Asi observamos la rtendencia y los ciclos
plot(airmiles)
abline(reg=lm(airmiles~time(airmiles)))

cycle(airmiles)

#PROMEDIO
plot(aggregate(airmiles,FUN=mean))

#bos plot regresion millas areas por cada mes de todos los años
boxplot(airmiles~cycle(airmiles))

#En  un intendo de visualizar mejor los datos usamos logaritmo (continua) y graficaos de nuevo los datos

#funcion logartimica se transforma una funcion continua , donde los maximos y nimmos van a ser identiciados
plot(log(airmiles),main="datos",xlab="Año",ylab="log(arimiles)")
plot(log(airmiles),main="datos",x_lab="Año",y_lab="log(arimiles)")
grid()
#Observamos a otra escala la tendencia y la estacionalidad
#

#Modelo de HOlt -winters
modelo_HW=HoltWinters(log(airmiles),seasonal = "additive")
plot(modelo_HW,main='Ajuste con Hot-Winters',xlab="Año", ylab='log (airmiles')
pormedio del afuncion logartimo


#Se puede observarque holtwinters  es una nueva serie
#en teoria esta muy cercana ala serie original


# ahora bien se debe descomponer  de acuerdo a sus caracteristicas
plot(fitted(modelo_HW),main="descomposicion con HW", xlab="Año", ylab='logairmiles')


#algo muy importante se puede predecir cone ste metodo de HoltWinters

pred=predict(modelo_HW,4,prediction.interval = TRUE)
pred

plot(modelo_HW,pred)
# estos valores osn poco creibles porque parte de la recursividad del valoranterior



###suavizado exponencial

#La funcion ses suavisado expoencial simole , de l alibreria forecast , es un tipo de promedio
#pnderado que estima el valor futuro en funcion del pronostico anerior mas un porcentaje de eoor pronosticado

library(forecast)
fit_ses1<- ses(log(airmiles),h=1,initial='simple',alpha = 0.1)
fit_ses1

#


