library(data.table)
fread("sample_submission.csv")

library(ISLR)
data("Weekly")
names(Weekly)
attach(Weekly)

plot(Volume~Year)

summary(Weekly)

pairs(Weekly)
cor(Weekly[,-ncol(Weekly)])

#Obvious correlation between volume and year

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly ,family=binomial)
summary(glm.fit)
#Lag2 has obvious importance / effect

