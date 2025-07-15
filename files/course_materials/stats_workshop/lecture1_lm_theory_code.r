library(tidyverse)
library(lattice)
library(multcomp)
library(emmeans)
library(MASS)
library(car)


#######################
## Linear regression ##
#######################

# Use the built-in data set cars to explore relationships #
# between stopping distance and speed #

data(cars)

# Preliminary look at the data#
head(cars)
plot(dist ~ speed, data = cars)

#Fit linear regression#
mod_cars <- lm(dist ~ speed, data = cars)

#Examine the model#
summary(mod_cars)

#Confidence interval of parameters#
confint(mod_cars, level = 0.95)

#Fitted mean and prediction#
mod_cars_fit <- predict(mod_cars, interval = "confidence", se.fit = T)
mod_cars_pred <- predict(mod_cars, newdata = data.frame(speed = c(5, 10)), 
  interval = "confidence", se.fit = T)


##########################
## Analysis of Variance ##
##########################

#Use data set PlantGrowth for one-way ANOVA#
plot(weight ~ group, data= PlantGrowth)

#Fitting ANOVA model#
mod_plant <- lm(weight ~ factor(group), data = PlantGrowth)
summary(mod_plant)

#Hypothesis testing#
#Testing equality of means#

#use anova() to test individual effects and interactions#
anova(mod_plant)

#use lht to test any linear hypothesis by specifying the hypothesis matrix#
lht(mod_plant, hypothesis.matrix = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = T))

#use model comparison to test effects of variables#
mod_plant_reduced <- lm(weight ~ 1, data = PlantGrowth)
anova(mod_plant, mod_plant_reduced)


# Use the data set ToothGrowth for a more complex ANOVA #

# Preliminary examination of the data#
ggplot(data = ToothGrowth) +
  geom_point(mapping = aes(x = dose, y = len, col = supp))
  
xyplot(len ~ supp | factor(dose), data = ToothGrowth)


#Analysis of variance#
#Here, we treat both dose and supp as categorical variable#
mod_tooth <- lm(len ~ supp * factor(dose), data = ToothGrowth)
summary(mod_tooth)

# Marginal means across each group#
emm_tooth <- emmeans(mod_tooth, specs = ~ supp * factor(dose) )
summary(emm_tooth)


# Hypothesis testing of each factor#
anova(mod_tooth)

# Are the effects of dose linear?#
mod_tooth_linear <- lm(len ~ supp * dose, data = ToothGrowth)
anova(mod_tooth, mod_tooth_linear)



