library(tidyverse)
library(car)
library(emmeans)
library(MASS)

#Contrast coding in linear models#

mod_trt_coding <- lm(len ~ supp, data = ToothGrowth)
summary(mod_trt_coding)

mod_sum_coding <- 
  lm(len ~ supp, contrasts = list(supp = contr.sum), data = ToothGrowth)
summary(mod_sum_coding)


#Use emmeans to see marginal means#

mod_tooth <- lm(len ~ supp * dose, data = ToothGrowth)
EMM_tooth <- emmeans(mod_tooth, spec = "supp")
  

#Type I, II, III sum of squares#
ToothGrowth_reduced <- ToothGrowth[-c(1:3), ] %>% mutate(dose_factor = factor(dose))

#Type I ANOVA depends on order#
mod1 <- lm(len ~ dose_factor * supp, data = ToothGrowth_reduced)
mod2 <- lm(len ~ supp * dose_factor, data = ToothGrowth_reduced)
anova(mod1)
anova(mod2)

#Type III anova#
mod3 <- lm(len ~ dose_factor * supp, data = ToothGrowth_reduced, 
  contrasts = list(supp = contr.sum, dose_factor = contr.sum))
summary(mod3)
Anova(mod3, type = 3)
lht(mod3, hypothesis.matrix = rbind(c(0,0,0,1,0,0)))

#Multiple comparisons#
EMM_mod3 <- emmeans(mod3, spec = "dose_factor") 
pairs(EMM_mod3, adjust = 'tukey')
contrast(EMM_mod3, method = "pairwise", adjust = "tukey")

#Model diagnostics#
mod_diagnostic <- lm(len ~ dose * supp, data = ToothGrowth)
boxcox(mod_diagnostic)
quartz(w = 5, h = 5)