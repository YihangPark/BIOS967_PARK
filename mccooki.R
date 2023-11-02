library(tidyverse)

mccooki <- read.csv("data/mccooki.csv")
mccooki

library(tidyverse)

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
colnames(mccooki)



summary(model1)
