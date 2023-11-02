mccooki <- read.csv("data/mccooki.csv")
mccooki
model1 <- glm(Copsuccess ~ Fcond + Cond..Lt.S. + Fcond*Cond..Lt.S., family=binomial(link="logit"),data=mccooki)

mccooki$Copsuccess = as.factor(mccooki$Copsucces)
mccooki$Fcond = as.factor(mccooki$Fcond)
mccooki$ Cond..Lt.S. = as.factor(mccooki$ Cond..Lt.S.)

summary(model1)
