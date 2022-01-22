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