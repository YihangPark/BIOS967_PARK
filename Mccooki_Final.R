
mccooki <- read.csv("data/mccooki.csv")
mccooki
library(tidyverse)
library(dplyr)

# due to the disorganized coding of the visual (light) and vibratory (substrate) signaling environment on the raw data file in a single column, separate the light and substrate variables into distinct columns
#use mutate function from dplyr package to make new variables from input data using vectorized functions
#make copsuccess
mccooki=mccooki %>% 
  mutate(light.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=1, end=1)) %>% 
  mutate(substrate.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=3, end=3))

mutate(cop.success=(X1st.cop.Time!="")+0) 


#change weird column name
colnames(mccooki)[colnames(mccooki) == "X1st.Cop.Male"] ="firstcop.male"
#factor
mccooki$firstcop.male <-as.factor (mccooki$firstcop.male)
mccooki$Fcond <-as.factor (mccooki$Fcond)
mccooki$light.condition <-as.factor(mccooki$light.condition)
mccooki$substrate.condition <-as.factor(mccooki$substrate.condition)
mccooki$Cond..Lt.S. <-as.factor (mccooki$Cond..Lt.S.)


#discard NAs in response variable-to make it 2-levels factor (LD vs HD)
mccooki2 <- mccooki %>% 
  filter(!firstcop.male=="na") %>% 
  mutate(firstcop.male = factor(firstcop.male))



#glm: Did the signaling environment and/or female diet influence copulation success in the simultaneous choice test?
glm.copsuccess <- glm(cop.success~ Fcond*light.condition*substrate.condition, family=binomial(link="logit"),data=mccooki2)
glm.copsuccess2 <- glm(cop.success~ Fcond + light.condition*substrate.condition, family=binomial(link="logit"),data=mccooki2)
glm.copsuccess3 <- glm(cop.success~ light.condition, family=binomial(link="logit"),data=mccooki2)
summary(glm.copsuccess3)
step(glm.copsuccess)
?glm









#Model 2&3 -question: Did the signaling environment and/or female diet influence the female choice between high- and low-diet males for copulation?
#let's run multinomial logistic regression analysis  where the response variable is 1st copulated males categorized by three factors: "high/low diet/NA."
#model3 <-multinom(firstcop.male ~ Fcond *light.condition * substrate.condition, data=mccooki)
#instead, you shoud do this to separately test the effects of excluding Fcond only
library(nnet)
model1 <-multinom(firstcop.male ~ light.condition ,data=mccooki2)
model2 <-multinom(firstcop.male ~ substrate.condition ,data=mccooki2)
model3 <-multinom(firstcop.male ~ light.condition + substrate.condition ,data=mccooki2) 
model4 <-multinom(firstcop.male ~ light.condition * substrate.condition ,data=mccooki2)
model5 <-multinom(firstcop.male ~ Fcond + light.condition * substrate.condition, data=mccooki2)
model6 <-multinom(firstcop.male ~ Fcond * light.condition * substrate.condition ,data=mccooki2)


#Choose a model by AIC in a stepwise algorithm
AIC.Bestfit <-step(model6)
summary(AIC.Bestfit)
##Getting p-value from model 2&3.
## Wald-Z test vs LRT to choose the best predictor?
#First trial: 2-tailed Wald-Z test to test significance of coefficients?
#
z <- summary(model6)$coefficients/summary(model6)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#install.packages("car")
library(car)
car::Anova(model6)

#likelihood ratio test to manually compare the fit of models 
library(lmtest)
lrtest(model1, model3) #significant substrate condition
lrtest(model2, model3) #non-significant effects of light condition
lrtest(model3, model4) #non-significant effects of interaction of environment
lrtest(model4, model5) #non-significant effects of female condition


?step

# Plotting the two-colored bar graph


##figure 1: Does the condition of the female and the signaling environment influence copulation success or female receptivity?

library(ggplot2)

mccooki$Cond..Lt.S.<- factor(mccooki$Cond..Lt.S., levels= c("+/+", "+/-", "-/+", "-/-"))

mccooki <- mccooki %>% 
  group_by(Cond..Lt.S., Fcond) %>% 
  mutate(Proportion = mean(cop.success))

ggplot(data=mccooki, aes(x = Cond..Lt.S., y = Proportion, fill = Fcond)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs( x= "Signaling environment (Light/Substrate)", y= "Proportions of Trials with Copulation", fill= "Female Diet")+
  scale_fill_discrete(labels=c("h" = "High", "l" = "Low"))+
  ylim(0,1)


## main figure: Do female choice vary by signaling environment and male quality?
mccooki2$Cond..Lt.S.<- factor(mccooki2$Cond..Lt.S., levels= c("+/+", "+/-", "-/+", "-/-"))
ggplot(data=mccooki2, aes(x = Cond..Lt.S., y = cop.success, fill = firstcop.male)) +
  geom_col( position = "fill") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")+
  labs( x= "Signaling environment (Light/Substrate)", y= "Proportions of Trials with Copulation", fill= "Male Diet")+
  scale_fill_discrete(labels=c("h" = "High", "l" = "Low"))


