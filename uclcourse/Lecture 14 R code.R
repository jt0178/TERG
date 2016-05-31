#Log-rank test
survdiff(Surv(melanom$days,melanom$status==1)~melanom$sex)

#Statified test controlling for confounders
survdiff(Surv(melanom$days,melanom$status==1)~melanom$sex+strata(melanom$ulc))
#plotting
kmstrata <- survfit(Surv(melanom$days,melanom$status==1) ~ melanom$sex+strata(melanom$ulc))
summary(kmstrata,censored=T)
plot(kmstrata, col=c("red", "blue", "darkgreen", "grey"))
#legend: sex=1 (women) and ulc=1 (present) come first, 
#so curve order is are (1,1), (1,2), (2,1), (2,2)
legend(locator(n=2),
       legend=c("women, present", "women, absent","men, present", "men, absent"), 
       pch=16, col=c("red", "blue", "darkgreen", "grey"))

survdiff(Surv(melanom$days,melanom$status==1)~melanom$ulc+strata(melanom$sex))
table(melanom$ulc,melanom$sex)

#Cox regression
summary(coxph(Surv(melanom$days,melanom$status==1)~melanom$sex))

#plotting groups separately  

#create dataframe with intruction for two sexes
curves <- data.frame(sex=c(1,2))
#create survival trajectories with survfit
svfit <- survfit(coxph(Surv(melanom$days,melanom$status==1)
          ~melanom$sex),newdata=curves)
plot(svfit)




