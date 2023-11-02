mccooki <- read.csv("C:/Users/user/Documents/GitHub/BIOS967_PARK/data/mccooki.csv")
mccooki
model1 <- glm(Copsuccess ~ Fcond + Cond..Lt.S. + Fcond*Cond..Lt.S., family=binomial,data=mccooki, (link="logit"))

mccooki$Copsuccess = as.factor(mccooki$Copsucces)
mccooki$Fcond = as.factor(mccooki$Fcond)
mccooki$ Cond..Lt.S. = as.factor(mccooki$ Cond..Lt.S.)


model1 <- glm(Copsuccess ~ Fcond + Cond..Lt.S. + Fcond*Cond..Lt.S., family=binomial,data=mccooki, (link="logit"))
?glm
