
mccooki <- read.csv("data/mccooki.csv")
library(tidyverse)
library(dplyr)
library(ggplot2)

# due to the disorganized coding of the visual (light) and vibratory (substrate) signaling environment on the raw data file in a single column, separate the light and substrate variables into distinct columns
#use mutate function from dplyr package to make new variables from input data using vectorized functions
mccooki=mccooki %>% 
  mutate(light.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=1, end=1)) %>% 
  mutate(substrate.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=3, end=3)) %>%
  mutate(cop.success=(X1st.cop.Time!="")+0)


#Model 1- Question: Did the signaling environment and/or female diet influence copulation success in the simultaneous choice test?
#Dai:Don't need to redundantly add separate terms and interaction terms in the model: this is enough-adding interaction terms automatically shows the effects of each terms as well
model1 <- glm(cop.success~ Fcond*light.condition*substrate.condition, family=binomial(link="logit"),data=mccooki)
summary(model1)
anova(model1, test="Chisq") #to get overall effects of factors (without reference level) from glm, use the argument test="Chisq"



#let's run multinomial logistic regression analysis  where the response variable is 1st copulated males categorized by three factors: "high/low diet/NA."
colnames(mccooki)[colnames(mccooki) == "X1st.Cop.Male"] ="firstcop.male" #change weird column name
colnames(mccooki)
mccooki$firstcop.male <-as.factor (mccooki$firstcop.male)
mccooki$Fcond <-as.factor (mccooki$Fcond)
mccooki$light.condition <-as.factor(mccooki$light.condition)
mccooki$substrate.condition <-as.factor(mccooki$substrate.condition)
mccooki$Cond..Lt.S. <-as.factor (mccooki$Cond..Lt.S.)

#discard NAs in response variable-to make it 2-levels factor (LD vs HD)
mccooki2 <- mccooki %>% 
  filter(!firstcop.male=="na") %>% 
  mutate(firstcop.male = factor(firstcop.male))



#Model 2&3 -question: Did the signaling environment and/or female diet influence the female choice between high- and low-diet males for copulation?

#model3 <-multinom(firstcop.male ~ Fcond *light.condition * substrate.condition, data=mccooki)
#20231128 Dai: The comparison between model 2 and 3 using a likelihood ratio test isn't suitable in this scenario. The current variables in model 3 encompass (Fcond + light.condition + substrate.condition + Fcond*light.condition + Fcond*substrate.condition + Fcond*light.condition*substrate.condition). If you proceed this way, you'll end up removing all four variables that involve Fcond*something.
#instead, you shoud do this to separately test the effects of excluding Fcond only
library(nnet)
model1 <-multinom(firstcop.male ~ light.condition ,data=mccooki)
model2 <-multinom(firstcop.male ~ substrate.condition ,data=mccooki)
model3 <-multinom(firstcop.male ~ light.condition + substrate.condition ,data=mccooki) 
model4 <-multinom(firstcop.male ~ light.condition * substrate.condition ,data=mccooki)
model5 <-multinom(firstcop.male ~ Fcond + light.condition + substrate.condition, data=mccooki)
model5 <-multinom(firstcop.male ~ Fcond + light.condition * substrate.condition ,data=mccooki)
model6 <-multinom(firstcop.male ~ Fcond * light.condition ,data=mccooki2)
model7 <-multinom(firstcop.male ~ Fcond * light.condition * substrate.condition ,data=mccooki2)

#Choose a model by AIC in a stepwise algorithm
model8 <-step(model7)

##Getting p-value from model 2&3.
## Wald-Z test vs LRT to choose the best predictor?
#First trial: 2-tailed Wald-Z test to test significance of coefficients?
#
z <- summary(model7)$coefficients/summary(model7)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


#Second trial: likelihood ratio test to compare the fit of model 2&3 
#The results indicate that the complete model did not demonstrate a better fit, suggesting that female diet did not have a significant effect on female choice between high- and low-diet males in a simultaneous choice test??
#install.packages("lmtest")
library(lmtest)
lrtest(model1, model3)
lrtest(model3, model4)
lrtest(model3, model5)
lrtest(model6, model7)
install.packages("car")
library(car)
car::Anova(model7)
car::Anova(model8)
#Likelihood ratio test
#Df  LogLik Df  Chisq Pr(>Chisq)
#1  16 -89.357                     
#2   8 -95.485 -8 12.254     0.1402


#using a different way to run LRT?
#no significant effects of either the signaling environment or the female diet on female choice?
#install.packages("MASS")
library(MASS)
summary(model7)
confint(model7)
MASS::dropterm(model7, trace=FALSE, test="Chisq")
#Df    AIC    LRT Pr(Chi)  
#<none>                 201.87                 
#Fcond                2 203.31 5.4465 0.06566 .
#light.condition      2 197.91 0.0433 0.97858  
#substrate.condition  2 202.86 4.9909 0.08246 .



## let's wrangle for the plot
#filter the signaling environment for rows
filter(mccooki, Cond..Lt.S.=="+/+")
filter(mccooki, Cond..Lt.S.=="+/-")
filter(mccooki, Cond..Lt.S.=="-/+")
filter(mccooki, Cond..Lt.S.=="-/-")
mccooki %>% select(Cond..Lt.S., firstcop.male) %>% 



# Plotting the two-colored bar graph
ggplot(data=mccooki2, aes(x = Cond..Lt.S., y = cop.success, fill = firstcop.male)) +
  geom_col( position = "fill") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")+
  labs( x= "Signaling environment (Light/Substrate)", y= "Proportions of Trials with Copulation", fill= "Male Diet")+
  scale_fill_discrete(labels=c("h" = "High", "l" = "Low"))