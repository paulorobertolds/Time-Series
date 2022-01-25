#Rotina do R

#rm(list=all())
#ler os dados
options(digits=4)
# Arquivo pibtri.txt disponível em https://github.com/paulorobertolds/Time-Series/blob/main/pibtri.txt
pibbr <- read.table("~/Downloads/dados/pibtri.txt", quote="\"", comment.char="")
#Arquiv igpdi.txt disponível em https://github.com/paulorobertolds/Time-Series/blob/main/igpditri.txt
igpdio <- read.table("~/Downloads/dados/igpditri.txt", quote="\"", comment.char="")

# Ver as primeiras observações e o tipo das variáveis
head(pibtri)
str(pibtri)
head(igpdio)
str(igpdio)

# Padronizar o deflator
igpdi = igpdio / igpdio[75,1]

# deflacionamento
pibbrr = pibbr/igpdi
pibbrr = ts(pibbrr, start=c(1995,1), frequency=4)

#plotando a série
plot.ts(pibbrr)

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

#Teste de raiz unitária
#(teste de Dickey-Fuller aumentado)
#H0: os dados não são estacionários
library(urca)
ur.pibrs<-ur.df(pibbrr,type="drift",selectlags="AIC")
summary(ur.pibrs)
ur.crescimentopibs<-ur.df(diff(log(pibbrr))*100,type="drift",selectlags="AIC")
summary(ur.crescimentopibs)

#Decomposição da série
pib.dec<-decompose(pibbrr)
plot(pib.dec)

