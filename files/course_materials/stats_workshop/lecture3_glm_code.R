library(car)
library(faraway)
library(MASS)


#Logistic regression#

#Basics of fitting logistic regression#
#Orings dataset from Challenger diaster#
head(orings)

#Visualizing damage rate and temperature relationship#
plot(I(damage / 6) ~ temp, data = orings, ylim = c(0, 1), xlim = c(25, 85))

#Compare different link functions#
m1_logit <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, data = orings)
summary(m1_logit) #Wald test default output#

confint(m1_logit) #Profile likelihood based CI (Recommended)#
confint.default(m1_logit) #Wald test based CI#

#type = "response" predict the value of p#
#type = "link" predicts the value of log(1 / (1-p))#
m1_logit_fit <- predict(m1_logit, type = "response", newdata = data.frame(temp = seq(25, 85, 1)))
lines(m1_logit_fit ~ seq(25, 85, 1))

#Logistic regression with probit link#
m1_probit <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial(link = probit), data = orings)
summary(m1_probit)

#In general, different links usually produces similar fit#
m1_probit_fit <- predict(m1_probit, type = "response", newdata = data.frame(temp = seq(25, 85, 1)))
lines(m1_probit_fit ~ seq(25, 85, 1), col = "red")


#Alternative way of specifying logistic regression#
m1_logit_2 <- glm(I(damage / 6) ~ temp, data = orings, family = binomial, 
  weights = rep(600, nrow(orings)))
#Different ways of specifying the model give the same results#
summary(m1_logit)
summary(m1_logit_2)

#Hypothesis testing in logistic regression#
#Use baby food data set to demonstrate hypothesis test#

head(babyfood)

#Saturated model: model with as many parameters as possible#
m2_sat <- glm(cbind(disease, nondisease) ~ sex * food, data = babyfood, family = binomial)
m2 <- glm(cbind(disease, nondisease) ~ food + sex, data = babyfood, family = binomial)
#Null model: model with only an intercept#
m2_null <- glm(cbind(disease, nondisease) ~ 1, data = babyfood, family = binomial)


#Various tools for hypothesis test#
#deviance based test#
#If hypothesis can be expressed as comparison of two nested models#
#use anova() for hypothesis test#
#Residual deviance#
anova(m2_sat, m2)
#Null deviance#
anova(m2, m2_null)

#Deviance based test can also be implemented in Anova()#
#Use sum to zero contrasting coding is required#
m2_sum <- glm(cbind(disease, nondisease) ~ food + sex, data = babyfood, family = binomial,
  contrasts = list(food = contr.sum, sex = contr.sum))
Anova(m2_sum, type = 3)

#Wald test#
lht(m2, hypothesis.matrix = rbind(c(0,0,0,1)))
lht(m2, hypothesis.matrix = rbind(c(0,1,0,0), c(0,0,1,0)))


#Inverse regression problem#
m3 <- glm(cbind(dead, alive) ~ conc, family = binomial, data = bliss)
#At what concentration does death rate reaches 50%?#
dose.p(m3, p = 0.5)


#Poisson regression or log linear model#

m4 <- glm(Species ~ Elevation + Nearest, family = poisson(link = log), data = gala)
#Examine residual deviance and its degree of freedom#
#If residual deviance far exceeds df, it indicate potential issue in model fitting#
summary(m4)


#Dealing with overdispersion#
#Use quasiliklihood for overdispersion#
m4_quasi <- glm(Species ~ Elevation + Nearest, family = quasipoisson, data = gala)
#Use negative binomial distribution for overdispersion#
m4_nb <- glm.nb(Species ~ Elevation + Nearest, data = gala)

#Rate models#
head(dicentric)

#Use log(cells) to correct / normalize ca count#
#Here, we use log(cells) because we use log link#
m5_offset <- glm(ca ~ doserate*factor(doseamt), family = poisson, data = dicentric, offset = log(cells))
summary(m5_offset)

