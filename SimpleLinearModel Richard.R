#load the data table faster
library(data.table)
DT <- fread("train.csv")
DT2 <- fread("test.csv")

#plot a linear regression between Ret_121 and Feature_1
lm(DT$Ret_121~DT$Feature_1)
fit <- lm(DT$Ret_121~DT$Feature_1)
plot(DT$Ret_121,DT$Feature_1)
abline(fit)


#Split training dataset into training/validation 
library(caret)
set.seed(100)
inTraining <- createDataPartition(DT$Ret_121, p = .75, list = FALSE)
inTraining
training <- DT[inTraining[, 1], ]
validation  <- DT[-inTraining[, 1], ]
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 2, 
  verboseIter = T
  ## repeated ten times
  #repeats = 10
)

#predictors <- grep("Ret_[1-9]", names(DT), value=T)[1:119]

training <- na.omit(training)

pr <- c(grep("Ret_[1-9]", names(DT2), value=T)[1:60])

DT2 = na.omit(DT2)
resDT <- data.table(data.frame(remove_it=DT2$Ret_2))

#Loop 
for(n in pr){
  #Random Force model
  fit <- train(training[, .SD, .SDcols=grep("Ret_[1-9]", names(training), value=T)[1:119]], training[, get(n)],
               method = "rf",
               trControl = fitControl,
               ## This last option is actually one
               ## for gbm() that passes through
               verbose = FALSE)
  fit
  plot(fit)
  validation = na.omit(validation)
  
  #Validation of the data
  out <- predict.train(fit, newdata=
                         validation[, .SD, .SDcols=grep("Ret_[1-9]", names(validation), value=T)[1:119]])
  
  plot(out)
  plot(validation$Ret_121)
  
  s_out <- sign(out) == sign(validation$Ret_121)
  table(s_out)
  
  cor(out, validation$Ret_121)
  
  predict.train(fit, newdata=
                  DT2[, .SD, .SDcols=grep("Ret_[1-9]", names(DT2), value=T)[1:119]])

  res_temp <- predict.train(fit, newdata=
                  DT2[, .SD, .SDcols=grep("Ret_[1-9]", names(DT2), value=T)[1:119]])
  
  resDT[, eval(n):=res_temp]
}

resDT[, remove_it:=NULL]