#Rotina do R

#ler os dados
options(digits=4)
pibbr=read.delim('https://github.com/paulorobertolds/Time-Series/blob/main/pibtri.txt', header= FALSE)
igpdio=read.delim('https://github.com/paulorobertolds/Time-Series/blob/main/igpditri.txt', header= FALSE)

igpdi = igpdio/igpdio[75,1]
# deflacionamento
pibbrr = pibbr/igpdi
pibbrr = ts(pibbrr, start=c(1995,1), frequency=4)
#logaritmizando os dados
pibbrrl = log(pibbrr)
# Retirando a última observação
pibbrrlt = pibbrrl[-length(pibbrrl)]
length(pibbrrlt)
pibbrrlt=ts(pibbrrlt,start=c(1995,1),frequency=4)
#Retirando as três últimas observações
pibbrrlt1 = pibbrrlt[-length(pibbrrlt)]
length(pibbrrlt1)
pibbrrlt2 = pibbrrlt1[-length(pibbrrlt1)]
length(pibbrrlt2)
pibbrrlt2=ts(pibbrrlt2,start=c(1995,1),frequency=4)
#modelo ETS(A,N,A)
library(forecast)
ets1=ets(pibbrrlt2)
nd_predets1<-forecast(ets1,h=3)
ets2014 = ets(pibbrrl)
fets2014=forecast(ets2014,h=5)
#modelo ETS(A,Ad,A)

hw1a=hw(pibbrrlt2,seasonal="additive", damped=TRUE)
nd_predhw1a<-ts(c(hw1a$mean[1],hw1a$mean[2],hw1a$mean[3]),start=c(2013,1), frequency=4)

hw1a2014 = hw(pibbrrl,seasonal="additive", damped=TRUE)
fhw1a2014=forecast(hw1a2014,h=5)
#modelo ETS(A,A,A)
hw2a=hw(pibbrrlt2,seasonal="additive", damped=FALSE)
nd_predhw2a<-ts(c(hw2a$mean[1],hw2a$mean[2],hw2a$mean[3]), +
                  start=c(2013,1), frequency=4)
hw2a2014 = hw(pibbrrl,seasonal="additive", damped=FALSE)
fhw2a2014=forecast(hw2a2014,h=5)
#modelo arima
aa2=auto.arima(pibbrrlt2)
nd_preda<-forecast(aa2, h=3)
arima2014 = auto.arima(pibbrrl)
fa2014=forecast(arima2014,h=5)
#modelo sarimax(1,1,0)x(0,0,1)
sarimax1=auto.arima(pibbrrlt2,xreg=pibmt2)
nd_predax<-forecast(sarimax1, h=3,xreg=c(0.90,1.22,1.35))
arimax2014 = auto.arima(pibbrrl,xreg=pibm)
fax2014=forecast(arimax2014,xreg=c(0.84,1.08,1.47,1.62,1))

#tabela 2 (descrição dos dados)
summary(pibbrr)
sqrt(var(pibbrr))
summary(diff(log(pibbrr))*100)
sqrt(var(diff(log(pibbrr))*100))
summary(pibm)
sqrt(var(pibm))
#Tabela 3 (teste de Dickey-Fuller aumentado)
library(urca)
ur.pibrs<-ur.df(pibbrr,type="drift",selectlags="AIC")
summary(ur.pibrs)
ur.crescimentopibs<-ur.df(diff(log(pibbrr))*100,type="drift",selectlags="AIC")
summary(ur.crescimentopibs)

taxaprevistaax2013
#tabelas 7 (previsões para 2014)
#taxa de crescimento em 2014 prevista pelo ETS*(A,N,A)
taxaprevistaets12014=(exp(fets2014$mean[4])-pibbrr[75])*100/pibbrr[75]
taxaprevistaets12014
#taxa de crescimento em 2014 prevista pelo ETS(A,Ad,A)
taxaprevistahw1a2014=(exp(fhw1a2014$mean[4])-pibbrr[75])*100/pibbrr[75]
taxaprevistahw1a2014
#taxa de crescimento em 2014 prevista pelo ETS(A,A,A)
taxaprevistahw2a2014=(exp(fhw2a2014$mean[4])-pibbrr[75])*100/pibbrr[75]
taxaprevistahw2a2014
#taxa de crescimento em 2014 prevista arima
taxaprevistaa2014=(exp(fa2014$mean[4])-pibbrr[75])*100/pibbrr[75]
taxaprevistaa2014
#taxa de crescimento em 2014 prevista pelo SARIMAX
taxaprevistaa2014=(exp(fax2014$mean[4])-pibbrr[75])*100/pibbrr[75]
taxaprevistaa2014
#figura 2 (modelos)
pdf(file="modelos.pdf")
par(mfrow=c(3,2))
plot(exp(pibbrrl),main="Modelo ETS(A,N,A)", xlab="tempo",ylab="PIB (milhões de R$, a preços de out/13)")
lines(exp(nd_predets1$mean),col=2,lwd=3)
lines(exp(fitted(ets1)),col=3,lwd=2)
legend("topleft",c("Observado","Previsão","Modelo"),bty="n",col=1:2:3,lwd=rep(1:3:2))
plot(exp(pibbrrl),main="Modelo ETS(A,Ad,A)",xlab="tempo",ylab="")
lines(exp(nd_predhw1a),col=2,lwd=3)
lines(exp(fitted(hw1a)),col=3,lwd=2)
legend("topleft",c("Observado","Previsão do Modelo","Modelo"),bty="n", col=1:2:3,lwd=rep(1:3:2))


plot(exp(pibbrrl),main="Modelo ETS(A,A,A)",xlab="tempo",ylab="PIB +
(milhões de R$, a preços de out/13)")
lines(exp(nd_predhw2a),col=2,lwd=3)
lines(exp(fitted(hw2a)),col=3,lwd=2)
legend("topleft",c("Observado","Previsão do Modelo","Modelo"),bty="n",+
         col=1:2:3,lwd=rep(1:3:2))
plot(exp(pibbrrl),main="Modelo ARIMA(1,1,0)",xlab="tempo",ylab="")
lines(exp(nd_preda$mean),col=2,lwd=3)
lines(exp(fitted(aa2)),col=3,lwd=2)
legend("topleft",c("Observado","Previsão do Modelo","Modelo"),bty="n",+
         col=1:2:3,lwd=rep(1:3:2))
plot(exp(pibbrrl),main="Modelo ARIMAX(1,1,0)x(0,0,1)",xlab="tempo",+
       ylab="PIB (milhões de R$, a preços de out/13)")
lines(exp(nd_predax$mean),col=2,lwd=3)
lines(exp(fitted(sarimax1)),col=3,lwd=2)
legend("topleft",c("Observado","Previsão do Modelo","Modelo"),bty="n",+
         col=1:2:3,lwd=rep(1:3:2))
dev.off()
#Figura 3 diagnóstico do modelo arima
arima2014 = auto.arima(pibbrrl)
summary(arima2014)
pdf(file="diagarima2014.pdf")
tsdiag(arima2014)
dev.off()
#Figura 4 diagnóstico do modelo Sarimax
#SARIMAX com pib mundial
arimax2014 = auto.arima(pibbrrl,xreg=pibm)
summary(arimax2014)
pdf(file="diagaapm.pdf")
tsdiag(arimax2014)
dev.off()
