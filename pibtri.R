pibbr=read.table('pibtri.txt', header= FALSE)
pibbr

igpdi=read.table('igpditri.txt', header= FALSE)
igpdi

igpdi = igpdi/igpdi[length(igpdi)]

# deflacionamento
pibbrr = pibbr/igpdi 

pibbrr = ts(pibbrr, start=c(1995,1), frequency=4)
pibbrr

# gráfico dos dados
plot(pibbrr)
# correlograma
acf(pibbrr) 
# correlograma parcial
pacf(pibbrr) 
summary(pibbrr)

sqrt(var(pibbrr))
summary(diff(log(pibbrr))*100)


#logaritmizando os dados
pibbrrl = log(pibbrr)
plot(pibbrrl)
acf(pibbrrl)
pacf(pibbrrl)
# correlograma da primeira diferença
acf(diff(pibbrrl)) 
# correlograma parcial da primeira diferença
pacf(diff(pibbrrl)) 

acf(diff(pibbrrl), lag.max=36)

#retirando a última observação
# última observação
pibbrr[length(pibbrr)] 
# última observação logaritmada
pibbrrl[length(pibbrrl)] 

# dado a ser predito
observadobr = pibbrr[length(pibbrr)]
observadobrlog = pibbrrl[length(pibbrrl)]
# série truncada
pibbrrlt = pibbrrl[-length(pibbrrl)] 
length(pibbrrlt)
pibbrrlt = ts(pibbrrlt, start=c(1995,1), frequency=4)
pibbrrlt

#alisamento exponencial
hw1 = HoltWinters(pibbrrlt, seasonal="additive")
predict(hw1,1)
prev.hw1=predict(hw1,1)[1]
prev.hw1
exp(prev.hw1)
(observadobr-exp(prev.hw1))/observadobr*100

hw2 = HoltWinters(pibbrrlt, seasonal="multiplicative", optim.start=c(0.257,0.015,0.293))
hw2
prev.hw2 = predict(hw2, 1)[1]
prev.hw2
# previsão HW multiplicativo
exp(prev.hw2) 
(observadobr-exp(prev.hw2))/observadobr*100


plot(hw1)
# gráfico dos componentes
plot(fitted(hw1)) 
plot(hw2)
# gráfico dos componentes
plot(fitted(hw2)) 

library(forecast)
hw3 = hw(pibbrrlt, h=6)
hw3[1]
hw3[2]$mean
exp(hw3[2]$mean[1])
(observadobr-exp(hw3[2]$mean[1]))/observadobr*100



hw4 = hw(pibbrrlt, damped=TRUE)
hw4[1]
hw4[2]$mean
exp(hw4[2]$mean[1])
observadobr
(observadobr-exp(hw4[2]$mean[1]))/observadobr*100
plot(hw3)
plot(hw4)

#Modelo SARIMA
arimafit1 = auto.arima(pibbrrlt)
arimafit1
forecast(arimafit1,h=1)$mean
exp(forecast(arimafit1,h=1)$mean)
observadobr
(observadobr-exp(forecast(arimafit1,h=1)$mean))/observadobr*100


#Modelo SARIMA airline
arimafit2 = arima(pibbrrlt, order=c(0,1,1), seasonal=list(order=c(0,1,1)))
summary(arimafit2)
(observadobr-exp(forecast(arimafit2,h=1)$mean))/observadobr*100
exp(forecast(arimafit2,h=1)$mean)



#dummies para época de crise
dummy = rep(1, length(pibbrrlt))
dummy[54:61] = 0
arimafit1d = arima(pibbrrlt, order=c(1,1,1), seasonal=list(order=c(0,0,1)),xreg=dummy, method="CSS-ML")
arimafit1d
exp(predict(arimafit1d, 1, newxreg=0)$pred)
(observadobr-exp(predict(arimafit1d, 1, newxreg=0)$pred))/observadobr*100
tsdiag(arimafit1d)


arimafit2d = arima(pibbrrlt, order=c(0,1,1), seasonal=list(order=c(0,1,1)),xreg=dummy, method="ML")
arimafit2d
exp(predict(arimafit2d, 1, newxreg=0)$pred)
(observadobr-exp(predict(arimafit2d, 1, newxreg=0)$pred))/observadobr*100
tsdiag(arimafit2d)

#decomposição em componentes
#require(graphics)
pib.stl = stl(pibbrrlt, "periodic")
plot(pib.stl)
pib.stl
max(pib.stl$time.series[, "trend"])
plot(pibbrrlt, xlab="tempo", ylab="PIB")
lines(pib.stl$time.series[, "trend"], lwd=1.3, col="blue")
title("Dados (linha preta) e tendência (linha azul)")



#direções para trabalhos adicionais
etsfit = ets(pibbrrlt)
accuracy(etsfit)
fcast <- predict(etsfit)
exp(predict(etsfit, 1)$mean)
(observadobr-exp(predict(etsfit, 1)$mean))/observadobr*100

#(observadobr-exp(prev.hw1))/observadobr*100
#(observadobr-exp(prev.hw2))/observadobr*100
#(observadobr-exp(hw3[2]$mean[1]))/observadobr*100
#(observadobr-exp(hw4[2]$mean[1]))/observadobr*100
#(observadobr-exp(forecast(arimafit1,h=1)$mean))/observadobr*100
#(observadobr-exp(forecast(arimafit2,h=1)$mean))/observadobr*100
#(observadobr-exp(predict(arimafit1d, 1, newxreg=0)$pred))/observadobr*100
#(observadobr-exp(predict(arimafit2d, 1, newxreg=0)$pred))/observadobr*100


#previsão para vários períodos
#forecast(arimafit1,h=5)$mean
#arima1=exp(forecast(arimafit1,h=5)$mean)
#plot(arima1)
#plot(fitted(arimafit1)) 



