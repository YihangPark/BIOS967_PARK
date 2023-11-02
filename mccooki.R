library(tidyverse)

mccooki <- read.csv("data/mccooki.csv")
mccooki

## separate light and substrate variables
mccooki = mccooki %>% 
  mutate(light.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=1, end=1)) %>% 
  mutate(substrate.condition=pull(mccooki, Cond..Lt.S.) %>% str_sub(start=3, end=3))

mccooki

model1 <- glm(Copsuccess ~ Fcond + Cond..Lt.S. + Fcond*Cond..Lt.S., family=binomial(link="logit"),data=mccooki)

mccooki$Copsuccess = as.factor(mccooki$Copsucces)
mccooki$Fcond = as.factor(mccooki$Fcond)
mccooki$ Cond..Lt.S. = as.factor(mccooki$ Cond..Lt.S.)

summary(model1)
