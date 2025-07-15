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

#Model diagnostic#
plot(mod_cars)

#Examine the model#
summary(mod_cars)

#Confidence interval of parameters#
confint(mod_cars)

#Fitted mean and prediction#
mod_cars_fit <- predict(mod_cars, interval = "confidence", se.fit = T)
mod_cars_pred <- predict(mod_cars, newdata = data.frame(speed = c(5, 10)), 
  interval = "prediction", se.fit = T)

#Hypothesis testing#
test_slope <- glht(mod_cars, linfct = matrix(c(0, 1), nrow = 1))




##########################
## Analysis of Variance ##
##########################

#Use data set PlantGrowth for one-way ANOVA#
plot(weight ~ group, data= PlantGrowth)

#Fitting ANOVA model#
mod_plant <- lm(weight ~ group, data = PlantGrowth)
summary(mod_plant)

#Hypothesis testing#
#Testing equality of means#
anova(mod_plant)
lht(mod_plant, hypothesis.matrix = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = T))
mod_plant_reduced <- lm(weight ~ 1, data = PlantGrowth)
anova(mod_plant, mod_plant_reduced)


#Pairwise comparison#
emm_plant <- emmeans(mod_plant, specs = ~ group)
summary(emm_plant)
pairs(emm_plant)
glht(mod_plant, linfct = matrix(c(0, 1, 1), nrow = 2, byrow = T)) %>% summary()




# Use the data set ToothGrowth for a more complex ANOVA #

# Preliminary examination of the data#
ggplot(data = ToothGrowth) +
  geom_point(mapping = aes(x = dose, y = len, col = supp))
  
xyplot(len ~ dose | supp, data = ToothGrowth)


# Analysis of variance#
mod_tooth <- lm(len ~ supp * factor(dose), data = ToothGrowth)
summary(mod_tooth)

# Marginal means and comparisons#
emm_tooth <- emmeans(mod_tooth, specs = ~ supp * factor(dose) )
summary(emm_tooth)


# Hypothesis testing #
anova(mod_tooth)

# Are the effects of dose linear?#
mod_tooth_linear <- lm(len ~ supp * dose, data = ToothGrowth)
anova(mod_tooth, mod_tooth_linear)



