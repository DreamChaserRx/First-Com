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
#Lag2 is statistically significant

coef(glm.fit)
summary(glm.fit)$coef
#Coefficients of the model

glm.probs=predict(glm.fit,type="response") 
contrasts(Weekly$Direction)
#Convert Probabilities to Up and Down

glm.pred=rep("Down","Up",nrow(Weekly))
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction )
#Calculate the probability of making right prediction