#random intercepts
polite <- lmer(frequency ~ attitude
               + (1|subject) + (1|scenario), data=pitch)
summary(polite)
coef(polite)
polite2 <- lmer(frequency ~ attitude + gender
               + (1|subject) + (1|scenario), data=pitch)
coef(polite2)
#significance test
polite.null <- lmer(frequency ~ gender + (1|subject) + (1|scenario),
                    data=pitch, REML=F)
polite.full <- lmer(frequency ~ attitude + gender
                + (1|subject) + (1|scenario), data=pitch, REML=F)
anova(polite.null, polite.full)

#random slopes model
pol.slope <- lmer(frequency~attitude+gender+(1+attitude|subject)
                    +(1+attitude|scenario),data=pitch, REML=F)
summary(pol.slope)
coef(pol.slope)

#mixed effects linear regression
plot(size ~ N,pch=rep(16:19,each=40),col=farm, data=farms)
summary(lm(size ~ N, data=farms))
mean.size <- data.frame(tapply(farms$size, farms$farm, mean))
mean.N <- data.frame(tapply(farms$N, farms$farm, mean))
plot(mean.size$tapply.farms.size..farms.farm..mean.
     ~mean.N$tapply.farms.N..farms.farm..mean., pch=16, cex=1.3)
summary(lm(mean.size$tapply.farms.size..farms.farm..mean.~mean.N$tapply.farms.N..farms.farm..mean.))

#separate regression lines
linear.models <- lmList(size~N|farm,data=farms)
summary(linear.models)
#random intercepts only
farm.int <- lmer(size ~ N+(1|farm), data=farms)
summary(farm.int)
coef(farm.int)
#random slopes
farm.slope <- lmer(size~N+(1+N|farm),data=farms)
summary(farm.slope)
coef(farm.slope)

#plotting
plot(size ~ N,pch=rep(16:19,each=40),col=farm, data=farms)
abline(85.82438, 0.69876, lwd=2)
xyplot(size ~ N, data=farms, type=c("p","r"),
       col=1:24, pch=16, cex=1.5, groups=farm,xlab="N", ylab="plant size")

#quiz plots
plot(ysimp~xsimp, pch=16, col=1:15[group], data=simp)
xyplot(ysimp ~ xsimp, groups=group,data=simp,
       pch=16, type=c("p", "r"), xab="", ylab="")



