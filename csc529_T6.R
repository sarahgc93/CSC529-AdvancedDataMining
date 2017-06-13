
####################
## 529_T6
library(e1071)
library(caret)

setwd('C:\\Users\\scummings\\Desktop\\MyFiles')
kidney<-read.csv("kidney.csv")
str(kidney)
prop.table(table(kidney$class))
summary(kidney)

pairs(kidney[1:6], col=kidney$class, cex=0.5)

#rain test
set.seed(1234)
ind <- sample(2, nrow(kidney), replace=TRUE, prob=c(0.8, 0.2))
train <- kidney[ind==1,]
test <- kidney[ind==2,]
dim(train)

#make a svm
svm_model <- svm(class ~ ., data=train)
summary(svm_model)

pred <- predict(svm_model,test)
t <- table(pred,test$class)
confusionMatrix(t)

help(svm)

#new linear svm
svm_model_lin <- svm(class ~ ., data=train,kernel="linear")
summary(svm_model_lin)

pred <- predict(svm_model_lin,test)
t <- table(pred,test$class)
confusionMatrix(t)

#new polynomial svm
svm_model_poly <- svm(class ~ ., data=train,kernel="polynomial")
summary(svm_model_poly)

pred <- predict(svm_model_poly,test)
t <- table(pred,test$class)
confusionMatrix(t)

#new radial svm
svm_model_radial <- svm(class ~ ., data=train,kernel="radial")
summary(svm_model_radial)

pred <- predict(svm_model_radial,test)
t <- table(pred,test$class)
confusionMatrix(t)

#new sigmoid svm
svm_model_sigmoid <- svm(class ~ ., data=train,kernel="sigmoid")
summary(svm_model_sigmoid)

pred <- predict(svm_model_sigmoid,test)
t <- table(pred,test$class)
confusionMatrix(t)

help(svm)


#The e1071 package has a convenient function called "tune.svm", 
#allowing us to tune several parameters at once:
  
mytunedsvm <- tune.svm(class ~ ., kernel = "polynomial", data = train, coef0 = (-1:4), degree = (1:4))
summary(mytunedsvm)

#plot the results.
plot (mytunedsvm,xlab="degree", ylab="coef0")

#new model
svm_model_tuned <- svm(class ~ ., data=train, kernel = "polynomial", coef0 = 2, degree = 3)
pred <- predict(svm_model_tuned,test)
t <- table(pred,test$class)
confusionMatrix(t)



