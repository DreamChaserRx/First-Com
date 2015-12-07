library(data.table)
fread("sample_submission.csv")

install.packages("ISLR")
library(ISLR)
data("Weekly")
names(Weekly)
attach(Weekly)

plot(Volume~Year)

summary(Weekly)

pairs(Weekly)
cor(Weekly[,-ncol(Weekly)])

#Obvious correlation between volume and year

