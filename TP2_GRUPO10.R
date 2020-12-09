#Modelo 1

library(dynlm)
library(lubridate)
library(dplyr)
library(stats)
library(forecast)
library(tidyverse)
library(ggplot2)
library(moments)

#Bibliotecas para cálculo de retornos
library(quantmod)
library(PerformanceAnalytics)

#Creamos el dataframe con la info del símbolo desde el 01/01/2014
getSymbols("AMZN",from='2014-01-01')
chartSeries(AMZN,theme=chartTheme("black"),name="Amazon.com Inc.")

#Cáculo de los retornos con el valor de cierre de cada día
AMZNReturn<-CalculateReturns(AMZN$AMZN.Close)
AMZNReturn[1]<-0 #Debemos cambiar por 0 el primer valor que es NA dado que de lo contrario surge un error al calcular la media
plot(AMZNReturn, main="Retornos AMZN")

#Histograma de los retornos ajustados a normal
chart.Histogram(AMZNReturn,methods=c('add.density','add.normal'),colorset=c('grey','red','blue'),xlab="Retornos",ylab="Densidad",main="Retornos AMZN")

#QQ-Plot para análisis de colas
qqnorm(AMZNReturn, xlab="Cuantiles teóricos",ylab="Cuantiles muestrales",main="Análisis de colas de retornos")
qqline(AMZNReturn, col=3)

#Cálculo de curtosis
kurtosis(AMZNReturn)

######################MODELOS GARCH(1,1)##########################################
library(rugarch)

#Modelo GARCH(1,1) con distribución normal de los residuos
#Definición de los parámetros
GARCHparam<- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        distribution.model='norm')
#Desarrollo del modelo
AMZNGARCH<-ugarchfit(data = AMZNReturn , spec = GARCHparam)
print(AMZNGARCH)
plot(ugarchfit(data = AMZNReturn , spec = GARCHparam), which = 9)

#Modelo con residuos T-Student
GARCHparam2<- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                         variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                         distribution.model='sstd')
AMZNGARCH2<-ugarchfit(data = AMZNReturn , spec = GARCHparam2)
print(AMZNGARCH2)
plot(ugarchfit(data = AMZNReturn , spec = GARCHparam2), which = 9)

#Modelo ARMA(1,1)-GARCH(1,1) con residos T-Student
GARCHparam3<- ugarchspec(mean.model=list(armaOrder=c(1,1)),
                        variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        distribution.model='sstd')
AMZNGARCH3<-ugarchfit(data = AMZNReturn , spec = GARCHparam3)
print(AMZNGARCH3)
plot(ugarchfit(data = AMZNReturn , spec = GARCHparam3), which = 9)


#Forecast
AMZNForecast<- ugarchforecast(fitORspec = AMZNGARCH3,
                             n.ahead= 10)
print(AMZNForecast)
par(mfrow=c(2,1))
plot (fitted(AMZNForecast),main="Forecast valor T+10")
plot (sigma(AMZNForecast), main="Forecast de la varianza")

#Asignación de cartera
par(mfrow=c(1,1))
v<-sqrt(252)*sigma(AMZNGARCH3)
w<-0.05/v
plot (merge(v,w),
      multi.panel=T,
      main="Asignación de porforlio")
tail(w)
