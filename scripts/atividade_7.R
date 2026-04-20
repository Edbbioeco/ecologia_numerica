library(vegan)
library(FD)
library(readr)
library(readxl)
setwd("G:/Meu Drive/R")
Ab<-read.table("Abund.txt",header=TRUE)
Fz<-read.table("Fuzzy.txt",header=TRUE)
ExDF <- dbFD(Fz, Ab,calc.CWM=FALSE)
#Exportando  o resultado:
write.csv(ExDF, file = "DFuncional.csv",row.names=TRUE)
pradarias<-c(rep("S",15),rep("C",15),rep("T",15))
fator<-factor(pradarias)
#riqueza funcional
boxplot(ExDF$FRic ~fator,border='black',col="green",las=1, xlab="pradarias", ylab="Riqueza", cex=1.4,cex.lab=1.4,cex.axis=1.2,main="Riqueza Funcional")
bartlett.test(ExDF$FRic ~fator) # Faça o teste de homogeneidade da variância
tapply(ExDF$FRic, fator, shapiro.test) # Teste a normalidade das amostras
R.anova1<-aov(ExDF$FRic ~fator)
summary(R.anova1)
TTeste1<-TukeyHSD(R.anova1)
plot(TTeste1)
#equitabilidade funcional
boxplot(ExDF$FEve ~fator,border='black',col="yellow",las=1, xlab="pradarias", ylab="equitabilidade", cex=1.4,cex.lab=1.4,cex.axis=1.2,main="Equitabilidade Funcional")
bartlett.test(ExDF$FEve ~fator) # Faça o teste de homogeneidade da variância
tapply(ExDF$FEve, fator, shapiro.test) # Teste a normalidade das amostras
R.anova2<-aov(ExDF$FEve ~fator)
summary(R.anova2)
TTeste2<-TukeyHSD(R.anova2)
plot(TTeste2)
#divergência funcional
boxplot(ExDF$FDiv ~fator,border='black',col="red",las=1, xlab="pradarias", ylab="divergência", cex=1.4,cex.lab=1.4,cex.axis=1.2,main="Divergência Funcional")
bartlett.test(ExDF$FDiv ~fator) # Faça o teste de homogeneidade da variância
tapply(ExDF$FDiv, fator, shapiro.test) # Teste a normalidade das amostras
R.anova3<-aov(ExDF$FDiv ~fator)
summary(R.anova3)
TTeste3<-TukeyHSD(R.anova3)
plot(TTeste3)
