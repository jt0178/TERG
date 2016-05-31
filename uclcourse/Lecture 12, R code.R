#factors with >2 levels
model.infant <- glm(healthy ~ as.factor(month),binomial, data=infant)
summary(model.infant)
coef(model.infant)
exp(coef(model.infant))

#interactions
model.chd <- glm(chd~age*cat*chl,binomial, data=evans)
summary(model.chd)

#model optimisation
model.menar4 <- glm(menarche~age*as.factor(tanner)+igf1,binomial, data=menar)
summary(model.menar4)
anova(model.menar4, test="Chisq")
drop1(model.menar4, test="Chisq")
model4 <- update(model4, ~. -age:as.factor(tanner))
summary(model4)
