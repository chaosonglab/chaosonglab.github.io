library(lme4)
library(pbkrtest)
library(lattice)
library(tidyverse)


## Fitting LMM based on design ##
head(cake)
m11 <- lmer(angle ~ recipe * temperature + (1 | replicate) + (1 | replicate:recipe), data = cake, REML = T)




#Understanding random effect specification#
head(Pastes)
m21 <- lmer(strength ~ (1|batch) + (1|batch:cask), data = Pastes, REML = T)
m22 <- lmer(strength ~ (1|batch) + (1|sample), Pastes, REML = T)
m23 <- lmer(strength ~ (1|batch/cask), data = Pastes, REML = T)



## Model Building for LMM ##
head(sleepstudy)

#m31 does not converge#
#m31 <- lmer(Reaction ~ Days + I(Days^2) + (Days + I(Days^2) | Subject), data = sleepstudy, REML = F)

m32 <- lmer(Reaction ~ Days + I(Days^2) + (Days | Subject), data = sleepstudy, REML = F)

#Uncorrelated slope and intercept for Days#
m321 <- lmer(Reaction ~ Days + I(Days^2) + (1 | Subject) + (0 + Days | Subject), data = sleepstudy, REML = F)

#These two models do not fit much worse than m32#
m33 <- lmer(Reaction ~ Days + I(Days^2) + (1 | Subject), data = sleepstudy, REML = F)
m34 <- lm(Reaction ~ Days + I(Days^2), data = sleepstudy)


#Hypothesis testing in LMM#
#likelihood ratio test#
m41 <- lmer(Reaction ~ Days + I(Days^2) + (Days | Subject), data = sleepstudy, REML = F)

#Test coef of days^2 = 0#
m42 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = F)
m42_1 <- lmer(Reaction ~ model.matrix(m41)[,-c(1, 3)] + (Days | Subject), sleepstudy, REML = F )
anova(m41, m42)

#Test coef of days^2 = 0.5#
m43 <- lmer(Reaction ~ Days + offset(0.5*Days^2) + (Days | Subject), data = sleepstudy, REML = F)
anova(m43, m41)

#Test coef of day + coef of days^2 = 8#
#reaction = intercetp + beta*days + (8-beta)*days^2#
#reaction = intercetp + beta*(days - days^2) + 8 * days^2#

m44 <- lmer(Reaction ~ I(Days-Days^2) + offset(8 * Days^2) + (Days | Subject), data = sleepstudy, REML = F)
anova(m41, m44)


#Safe in practice#
#Parametric bootstrapping#
PBmodcomp(m41, m42, n = 10000, cl = 10)
PBmodcomp(m41, rbind(c(0,0,1)), n = 500, cl = 5)
#Convert restriction matrix to model#
m42_2 <- restriction_matrix2model(m41, rbind(c(0,0,1)))


#Kenward-Roger adjustment on F-test#
KRmodcomp(m41, m42)
KRmodcomp(m41, rbind(c(0,0,1)))


#Satterthwait adjustment on F-test#
SATmodcomp(m41, m42)


library(lmerTest)
m41 <- lmer(Reaction ~ Days + I(Days^2) + (Days | Subject), data = sleepstudy, REML = F)