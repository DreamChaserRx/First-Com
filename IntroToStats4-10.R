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
mean(glm.pred==Direction)
#Calculate confusion matrix and the probability of making right prediction

train =( Year <= 2008)
Weekly.testing= Weekly [! train ,]
glm.fit=glm(Direction~Lag2,data=Weekly ,family=binomial, subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit,Weekly.testing,type="response")
glm.pred=rep("Down",nrow(Weekly.testing))
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Weekly.testing$Direction)
mean(glm.pred==Weekly.testing$Direction)
#Logistic Model, Using Lag2 as the only predictor,Prob=0.625

library(MASS)
lda.fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.fit
lda.class=predict(lda.fit,Weekly.testing)$class
table(lda.class,Weekly.testing$Direction)
mean(lda.class==Weekly.testing$Direction)
#Linear Discriminant Analysis,Prob=0.625

qda.fit = qda(Direction ~ Lag2, data=Weekly.testing, subset=train)
qda.fit
qda.class=predict(qda.fit,Weekly.testing)$class
table(qda.class,Weekly.testing$Direction)
mean(qda.class==Weekly.testing$Direction)
#Quadratic Discriminant Analysis,Prob=0.63

library(class)
train.X=Weekly[train,"Lag2",drop=F]
test.X=Weekly[!train,"Lag2",drop=F]
train.Direction=Weekly[train,"Direction",drop=T]
test.Direction=Weekly[!train,"Direction",drop=T]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=5)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)
#K-Nearest Neighbors,Prob=0.5 when k=1, Prob=0.62 when k=4