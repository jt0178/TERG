#install and load package 'survival'
#load package ISwR
melanom
#to make melanom visible
melanom <- melanom

#censoring
Surv(melanom$days, melanom$status==1)
km1 <- survfit(Surv(melanom$days, melanom$status==1) ~ 1)
summary(km1)
plot(km1)

text (1500,0.1, ">plot(km1)", col="red")
plot(km1, ylab="survival", xlab="age (years)", conf.int=F,
     mark.time=F, xscale=365)
text(6,0.2, ">plot(km1),ylab=''survival'',
  xlab=''age (years)'', conf.int=F, 
     mark.time=F, xscale=365",
     col="red", cex=1)

#by sex
kmsex <- survfit(Surv(melanom$days, melanom$status==1) ~ melanom$sex)
summary(kmsex)
plot(kmsex,conf.int=T, col=c("blue", "red"))
#legend with choice of location 
#don't forget to click on plot and finish
legend(locator(n=2),legend=c("men","women"), 
       pch=16, col=c("blue","red"))




#censoring
Surv(Agta$age, Agta$dead==1)
AgtaKM <- survfit(Surv(Agta$age,Agta$dead==1) ~ 1)
summary(AgtaKM)
summary(AgtaKM)$surv
plot(AgtaKM)

#by sex
AgtaSex <- survfit(Surv(Agta$age,Agta$dead==1) ~ Agta$SexID)
summary(AgtaSex)
plot(AgtaSex,conf.int=T, col=c("blue", "red"))
#legend with choice of location 
#don't forget to click on plot and finish
legend(locator(n=2),legend=c("men","women"), 
       pch=16, col=c("blue","red"))

#mortality
summary(AgtaKM)$surv -> Si
Si <- c(1, Si)
summary(AgtaKM)$surv -> S2
c(S2, 0) -> S2
mort <- 1-(S2/Si)
plot(log(mort) ~ seq(1,83,1))

#Kung
