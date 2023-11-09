
mccooki <- read.csv("data/mccooki.csv")
mccooki

library(tidyverse)
library(dplyr)
library(nnet)
install.packages("nnet")
## separate light and substrate variables
mccooki=mccooki %>% 
  mutate(light.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=1, end=1)) %>% 
  mutate(substrate.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=3, end=3)) %>%
  mutate(cop.success=(X1st.cop.Time!="")+0)

mccooki

model1 <- glm(cop.success~ Fcond + light.condition + substrate.condition + Fcond*light.condition + Fcond*substrate.condition + light.condition*substrate.condition, family=binomial(link="logit"),data=mccooki)
summary(model1)
anova(model1, test="Chisq") #to get overall effects of factors (without reference level) from glm, use the argument test="Chisq"

#my trial 20231102

mccooki=mccooki %>% 
  mutate_all(na_if, "")
mccooki[is.na(mccooki)] <- ""

#let's run multinomial logistic regression with 1st copulated males(high/low diet/NA; 3 factors) as response variables
colnames(mccooki)[colnames(mccooki) == "X1st.Cop.Male"] ="firstcop.male"
colnames(mccooki)
mccooki$firstcop.male <-as.factor (mccooki$firstcop.male)
mccooki$Fcond <-as.factor (mccooki$Fcond)
mccooki$light.condition <-as.factor(mccooki$light.condition)
mccooki$substrate.condition <-as.factor(mccooki$substrate.condition)

#this is redundant
model2 <-multinom(firstcop.male ~ Fcond +light.condition + substrate.condition + Fcond*light.condition + Fcond*substrate.condition + light.condition*substrate.condition + light.condition*substrate.condition*Fcond ,data=mccooki) 

#this is enough-Dai
model2 <-multinom(firstcop.male ~ Fcond *light.condition * substrate.condition ,data=mccooki) 
summary(model2)

model3 <-multinom(firstcop.male ~ light.condition*substrate.condition, data=mccooki)
summary(model3)

#tried to run two-tailed Z test and p-value
z <- summary(model3)$coefficients/summary(model3)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#let's make a messy tables with coefficient + p-value.....
rbind(model_summary2$coefficients[1,],model_summary2$standard.errors[1,],z[1,],p[1,])
rownames(mccooki) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(mccooki)

anova(model2)