#group comparisons
kruskal.test(rats$Glycogen ~ rats$Treatment)

avrat <- tapply(rats$Glycogen, list(rats$Treatment,rats$Rat), mean)
avrat
treat <- gl(3,1,length=6)
kruskal.test(as.vector(avrat) ~ treat)
anova(lm(as.vector(avrat) ~ treat))

#rat example
#turn variables into factors
TreatmentF <- factor(rats$Treatment)
LiverF <- factor(rats$Liver)
RatF <- factor(rats$Rat)

#nested levels
rat <- TreatmentF:RatF
liver <- TreatmentF:RatF:LiverF 
#model
modelrats <- lmer(Glycogen ~ TreatmentF+(1|rat)+(1|liver), data=rats)
summary(modelrats)
#significance
modelrats.full <- lmer(Glycogen ~ TreatmentF+(1|rat)+(1|liver), data=rats, REML=F)
summary(modelrats.full)
modelrats.null <- lmer(Glycogen ~ (1|rat)+(1|liver), data=rats, REML=F)
anova(modelrats.null, modelrats.full)
vars <- c(14.167,36.065,21.167)
100*vars/sum(vars)

#Multilevel modelling
attach(childfull)
d <- town:district
s <- town:district:factor(street)
h <- town:district:factor(street):house

#no fixed effect and four nested random effects:
schools.null <- lmer(response~ (1|town)+(1|d)+(1|s)+(1|h))
summary(schools.null)
#one fixed effect (gender) and four nested random effects:
schools.full <- lmer(response~gender+(1|town)+(1|d)+(1|s)+(1|h))
summary(schools)
#significance test
schools.gen <- lmer(response~gender+(1|town)+(1|d)+(1|s)+(1|h), REML=F)
schools.nogen <- lmer(response~(1|town)+(1|d)+(1|s)+(1|h), REML=F)
anova(schools.nogen, schools.gen)

#variance components analysis
vc <- c(4.0817, 15.6746, 168.3500, 36.9757, 36.2406)
vc <- 100*c(4.0817, 15.6746, 168.3500, 36.9757, 36.2406)/sum(vc)
vc

#plotting
hist(response[town=="Coventry"],main="Coventry",breaks=seq(40,150,5),
     xlab="response")
plot(response~district,subset=(town=="Coventry"),main="Coventry")

#generalised mixed effects
#logistic model
library("MASS")

table(bacteria$y,bacteria$trt)
#glm - not controlling for pseudoreplication
summary(glm(y~ trt, binomial, data=bacteria))
drop1(glm(y~ trt, binomial, data=bacteria))

#only ID as random effect
infection <- lmer(y~trt+(1|ID),family=binomial, data=bacteria)
summary(infection)
drop1(infection)
#significance
infection.a <- lmer(y~trt+(1|ID),family=binomial, data=bacteria, REML=F)
infection.b <- lmer(y~(1|ID),family=binomial, data=bacteria, REML=F)
anova(infection.b, infection.a)

#week and ID as random effect
infection2 <- lmer(y~trt+(week|ID),binomial, data=bacteria)
summary(infection2)

#significance
infection.c <- lmer(y~trt+(1|ID),family=binomial, data=bacteria, REML=F)
infection.d <- lmer(y~trt+(week|ID),family=binomial, data=bacteria, REML=F)
anova(infection.c, infection.d)



#proportion test not controlling for pseudoreplication
prop.test(c(12,18,13),c(96,62,62))
