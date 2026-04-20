#1ª questão
#Esuário 1
library(vegan)
library(readr)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
estuario1<-read.table("Estuário 11.txt",header = TRUE)
Estuario1<-data.frame(estuario1)
Estuario1<-Estuario1[order(Estuario1$Soma, decreasing = TRUE),]
LogEstuario1<- octav(Estuario1$Soma)
plot(LogEstuario1)
plot(LogEstuario1, prop = TRUE, border=NA, col=NA)
lines(LogEstuario1, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("espécies` macro"), col=c("red"), lty=1)
Rank1.1<-rad(Estuario1$Soma)
plot(Rank1.1, ylab="Número de indivíduos")
Els1 <- fitsad(Estuario1$Soma, "ls")
summary(Els1)
coef(Els1)
logLik(Els1)
AIC(Els1)
EstPerfilGram1<-profile(Els1)
likelregions(EstPerfilGram1)
confint(EstPerfilGram1)
plotprofmle(EstPerfilGram1)
plot(EstPerfilGram1)  
plot(Els1 ,which=1)
plot(Els1 ,which=2)
plot(Els1 ,which=3)
plot(Els1 ,which=4)
Epl1 <- fitsad(Estuario1$Soma, sad="poilog")
summary(Epl1)
Eln1 <- fitsad(Estuario1$Soma, sad="lnorm", trunc=0.5)
summary(Eln1)
AICtab(Els1,Epl1, Eln1, base=TRUE)
Ehist.ls1<- octavpred(Els1)
Ehist.pl1<-octavpred(Epl1)
Ehist.ln1<-octavpred(Eln1)
plot(LogEstuario1,border="black",col="lightgreen",ylab="abundância",xlab="classes", main="Estuário 1",ylim=c(0,17))+
  lines(Ehist.ls1, col="green")+
  lines(Ehist.pl1, col="blue")+
  lines(Ehist.ln1, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Ehistrad.ls1<- radpred(Els1)
Ehistrad.pl1<-radpred(Epl1)
Ehistrad.ln1<-radpred(Eln1)
plot(Rank1.1,ylab="abundância",xlab="rank", main="Estuário 1")+
  lines(Ehist.ls1, col="green")+
  lines(Ehist.pl1, col="blue")+
  lines(Ehist.ln1, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
#Esuário 2
library(vegan)
library(readr)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
estuario2<-read.table("Estuario 22.txt",header = TRUE)
Estuario2<-data.frame(estuario2)
Estuario2<-Estuario2[order(Estuario2$Soma, decreasing = TRUE),]
LogEstuario2<- octav(Estuario2$Soma)
plot(LogEstuario2)
plot(LogEstuario2, prop = TRUE, border=NA, col=NA,ylim=c(0,0.35))
lines(LogEstuario2, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("espécies` macro"), col=c("red"), lty=1)
Rank1.2<-rad(Estuario2$Soma)
plot(Rank1.2, ylab="Número de indivíduos")
Els2 <- fitsad(Estuario2$Soma, "ls")
summary(Els2)
coef(Els2)
logLik(Els2)
AIC(Els2)
EstPerfilGram2<-profile(Els2)
likelregions(EstPerfilGram2)
confint(EstPerfilGram2)
plotprofmle(EstPerfilGram2)
plot(EstPerfilGram2)  
plot(Els2 ,which=1)
plot(Els2 ,which=2)
plot(Els2 ,which=3)
plot(Els2 ,which=4)
Epl2 <- fitsad(Estuario2$Soma, sad="poilog")
summary(Epl2)
Eln2 <- fitsad(Estuario2$Soma, sad="lnorm", trunc=0.5)
summary(Eln2)
AICtab(Els2,Epl2, Eln2, base=TRUE)
Ehist.ls2<- octavpred(Els2)
Ehist.pl2<-octavpred(Epl2)
Ehist.ln2<-octavpred(Eln2)
plot(LogEstuario2,border="black",col="lightyellow",ylab="abundância",xlab="classes", main="Estuário 2",ylim=c(0,25))+
  lines(Ehist.ls1, col="green")+
  lines(Ehist.pl1, col="blue")+
  lines(Ehist.ln1, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Ehistrad.ls2<- radpred(Els2)
Ehistrad.pl2<-radpred(Epl2)
Ehistrad.ln2<-radpred(Eln2)
plot(Rank1.2,ylab="abundância",xlab="rank", main="Estuário 2")+
  lines(Ehist.ls2, col="green")+
  lines(Ehist.pl2, col="blue")+
  lines(Ehist.ln2, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)


#2ª questão
#Pradaria de Halodule wrightii
library(vegan)
library(readxl)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
d<-read_xlsx("Diversidade 1.xlsx")
D<-data.frame(d)
LogD1<-octav(D$Abundancia)
plot(LogD1)
plot(LogD1, prop = TRUE, border=NA, col=NA)
lines(LogD1, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("abundância"), col=c("red"), lty=1)
Rank2.1<-rad(D$Abundancia)
plot(Rank2.1, ylab="abundância das espécies")
Dls <- fitsad(D$Abundancia, "ls")
summary(Dls)
coef(Dls)
logLik(Dls)
AIC(Dls)
DPerfilGram<-profile(Dls)
likelregions(DPerfilGram)
confint(DPerfilGram)
plotprofmle(DPerfilGram)
plot(DPerfilGram)  
plot(Dls ,which=1)
plot(Dls ,which=2)
plot(Dls ,which=3)
plot(Dls ,which=4)
Dpl <- fitsad(D$Abundancia, sad="poilog")
summary(Dpl)
Dln <- fitsad(D$Abundancia, sad="lnorm", trunc=0.5)
summary(Dln)
AICtab(Dls,Dpl, Dln, base=TRUE)
Dhist.ls<- octavpred(Dls)
Dhist.pl<-octavpred(Dpl)
Dhist.ln<-octavpred(Dln)
plot(LogD1,border="black",col="lightyellow",ylab="abundância",xlab="classes", main="pradaria Halodule Wrightii",ylim=c(0,70))+
  lines(Dhist.ls, col="green")+
  lines(Dhist.pl, col="blue")+
  lines(Dhist.ln, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Dhistrad.ls<- radpred(Dls)
Dhistrad.pl<-radpred(Dpl)
Dhistrad.ln<-radpred(Dln)
plot(Rank2.1,ylab="abundância",xlab="rank", main="pradaria Halodule Wrightii")+
  lines(Dhistrad.ls, col="green")+
  lines(Dhistrad.pl, col="blue")+
  lines(Dhistrad.ln, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
#estuário poluido
library(vegan)
library(readxl)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
d2<-read_xlsx("Diversidade 2.xlsx")
D2<-data.frame(d2)
LogD2<-octav(D2$Abundancia.2)
plot(LogD2)
plot(LogD2, prop = TRUE, border=NA, col=NA)
lines(LogD2, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("abundância"), col=c("red"), lty=1)
Rank2.2<-rad(D2$Abundancia.2)
plot(Rank2.2, ylab="abundância das espécies")
Dls2 <- fitsad(D2$Abundancia.2, "ls")
summary(Dls2)
coef(Dls2)
logLik(Dls2)
AIC(Dls2)
DPerfilGram2<-profile(Dls2)
likelregions(DPerfilGram2)
confint(DPerfilGram2)
plotprofmle(DPerfilGram2)
plot(DPerfilGram2)  
plot(Dls2 ,which=1)
plot(Dls2 ,which=2)
plot(Dls2 ,which=3)
plot(Dls2 ,which=4)
Dpl2 <- fitsad(D2$Abundancia.2, sad="poilog")
summary(Dpl2)
Dln2 <- fitsad(D2$Abundancia.2, sad="lnorm", trunc=0.5)
summary(Dln2)
AICtab(Dls2,Dpl2, Dln2, base=TRUE)
Dhist.ls2<- octavpred(Dls2)
Dhist.pl2<-octavpred(Dpl2)
Dhist.ln2<-octavpred(Dln2)
plot(LogD2,border="black",col="orange",ylab="abundância",xlab="classes", main="estuário poluído",ylim=c(0,15))+
  lines(Dhist.ls2, col="green")+
  lines(Dhist.pl2, col="blue")+
  lines(Dhist.ln2, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Dhistrad.ls2<- radpred(Dls2)
Dhistrad.pl2<-radpred(Dpl2)
Dhistrad.ln2<-radpred(Dln2)
plot(Rank2.2,ylab="abundância",xlab="rank", main="esuário poluído")+
  lines(Dhistrad.ls2, col="green")+
  lines(Dhistrad.pl2, col="blue")+
  lines(Dhistrad.ln2, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
#peix recifal
library(vegan)
library(readxl)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
d3<-read_xlsx("diversidade 3.xlsx")
D3<-data.frame(d3)
LogD3<-octav(D3$Abundancia3)
plot(LogD3)
plot(LogD3, prop = TRUE, border=NA, col=NA)
lines(LogD3, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("abundância"), col=c("red"), lty=1)
Rank2.3<-rad(D3$Abundancia3)
plot(Rank2.3, ylab="abundância das espécies")
Dls3 <- fitsad(D3$Abundancia3, "ls")
summary(Dls3)
coef(Dls3)
logLik(Dls3)
AIC(Dls3)
DPerfilGram3<-profile(Dls3)
likelregions(DPerfilGram3)
confint(DPerfilGram3)
plotprofmle(DPerfilGram3)
plot(DPerfilGram3)  
plot(Dls3 ,which=1)
plot(Dls3 ,which=2)
plot(Dls3 ,which=3)
plot(Dls3 ,which=4)
Dpl3 <- fitsad(D3$Abundancia3, sad="poilog")
summary(Dpl3)
Dln3 <- fitsad(D3$Abundancia3, sad="lnorm", trunc=0.5)
summary(Dln3)
AICtab(Dls3,Dpl3, Dln3, base=TRUE)
AICtab(Dls3, Dpl3, Dln3, base=TRUE)
Dhist.ls3<- octavpred(Dls3)
Dhist.pl3<-octavpred(Dpl3)
Dhist.ln3<-octavpred(Dln3)
plot(LogD3,border="black",col="lightblue",ylab="abundância",xlab="classes", main="peixe recifal",ylim=c(0,20))+
  lines(Dhist.ls3, col="green")+
  lines(Dhist.pl3, col="blue")+
  lines(Dhist.ln3, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Dhistrad.ls3<- radpred(Dls3)
Dhistrad.pl3<-radpred(Dpl3)
Dhistrad.ln3<-radpred(Dln3)
plot(Rank2.3,ylab="abundância",xlab="rank", main="peixe recifal")+
  lines(Dhistrad.ls3, col="green")+
  lines(Dhistrad.pl3, col="blue")+
  lines(Dhistrad.ln3, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
#diptera associados a um cadáver
library(vegan)
library(readxl)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
d4<-read_xlsx("diversidade 4.xlsx")
D4<-data.frame(d4)
LogD4<-octav(D4$Abundancia4)
plot(LogD4)
plot(LogD4, prop = TRUE, border=NA, col=NA)
lines(LogD4, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("abundância"), col=c("red"), lty=1)
Rank2.4<-rad(D4$Abundancia4)
plot(Rank2.4, ylab="abundância das espécies")
Dls4 <- fitsad(D4$Abundancia4, "ls")
summary(Dls4)
coef(Dls4)
logLik(Dls4)
AIC(Dls4)
DPerfilGram4<-profile(Dls4)
likelregions(DPerfilGram4)
confint(DPerfilGram4)
plotprofmle(DPerfilGram4)
plot(DPerfilGram4)  
plot(Dls4 ,which=1)
plot(Dls4 ,which=2)
plot(Dls4 ,which=3)
plot(Dls4 ,which=4)
Dpl4 <- fitsad(D4$Abundancia4, sad="poilog")
summary(Dpl4)
Dln4 <- fitsad(D4$Abundancia4, sad="lnorm", trunc=0.5)
summary(Dln4)
AICtab(Dls4,Dpl4, Dln4, base=TRUE)
Dhist.ls4<- octavpred(Dls4)
Dhist.pl4<-octavpred(Dpl4)
Dhist.ln4<-octavpred(Dln4)
plot(LogD4,border="black",col="lightyellow",ylab="abundância",xlab="classes", main="dipetros associados a um cadáver",ylim=c(0,2.5))+
  lines(Dhist.ls4, col="green")+
  lines(Dhist.pl4, col="blue")+
  lines(Dhist.ln4, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Dhistrad.ls4<- radpred(Dls4)
Dhistrad.pl4<-radpred(Dpl4)
Dhistrad.ln4<-radpred(Dln4)
plot(Rank2.4,ylab="abundância",xlab="rank", main="dipetros associados a um cadáver")+
  lines(Dhistrad.ls4, col="green")+
  lines(Dhistrad.pl4, col="blue")+
  lines(Dhistrad.ln4, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
#fauna plataforma continetal
library(vegan)
library(readxl)
library(sads)
library(devtools)
setwd("C:/Users/Edson Junior/OneDrive/Documentos/")
d5<-read_xlsx("diversidade 5.xlsx")
D5<-data.frame(d5)
LogD5<-octav(D5$Abundancia5)
plot(LogD5)
plot(LogD5, prop = TRUE, border=NA, col=NA)
lines(LogD5, mid = FALSE, prop = TRUE, col="red")
legend("topright", c("abundância"), col=c("red"), lty=1)
Rank2.5<-rad(D4$Abundancia4)
plot(Rank2.5, ylab="abundância das espécies")
Dls5 <- fitsad(D5$Abundancia5, "ls")
summary(Dls5)
coef(Dls5)
logLik(Dls5)
AIC(Dls5)
DPerfilGram5<-profile(Dls5)
likelregions(DPerfilGram5)
confint(DPerfilGram5)
plotprofmle(DPerfilGram5)
plot(DPerfilGram4)  
plot(Dls5 ,which=1)
plot(Dls5 ,which=2)
plot(Dls5 ,which=3)
plot(Dls5 ,which=4)
Dpl5 <- fitsad(D5$Abundancia5, sad="poilog")
summary(Dpl5)
Dln5 <- fitsad(D5$Abundancia5, sad="lnorm", trunc=0.5)
summary(Dln5)
AICtab(Dls5,Dpl5, Dln5, base=TRUE)
Dhist.ls5<- octavpred(Dls4)
Dhist.pl5<-octavpred(Dpl5)
Dhist.ln5<-octavpred(Dln5)
plot(LogD5,border="black",col="lightgrey",ylab="abundância",xlab="classes", main="fauna na plataforma continental",ylim=c(0,25))+
  lines(Dhist.ls5, col="green")+
  lines(Dhist.pl5, col="blue")+
  lines(Dhist.ln5, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
Dhistrad.ls5<- radpred(Dls5)
Dhistrad.pl5<-radpred(Dpl5)
Dhistrad.ln5<-radpred(Dln5)
plot(Rank2.5,ylab="abundância",xlab="rank", main="fauna na plataforma continental")+
  lines(Dhistrad.ls5, col="green")+
  lines(Dhistrad.pl5, col="blue")+
  lines(Dhistrad.ln5, col="red")+
  legend("topright",c("modelo logarítimico","modelo lognormal","modelo lognormal- truncada"),col=c("green","blue","red"),lty = 1)
  
  
  