## CASE STUDY 1

library(caret)
library(rpart)
library(randomForest)
library(lmtest)
library(dplyr)
library(ROCR)

#read in cancerdata file
setwd("/Users/sarahcummings/Documents/csc529")
df<-read.csv("creditcard.csv")

#make sure class varaible is read in as factor
df$Class<-as.factor(df$Class)
levels(df$Class)

#summary
summary(df)
#compute correlations of all numeric varaibles
cor(df[1:30])

#see if there is a significant difference in means for the two classes
t.test(Amount~Class,data=df)

#check to see if this data could be modeled by a simple logistic regression
mylogit <- glm(Class~., data = df, family = "binomial")
predict <- predict(mylogit, type = 'response')
table(df$Class, predict > 0.5)

coeftest(mylogit)

#create a simple 70-30 split.
set.seed(1234)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]
table(train$Class)


#make a decision tree
tree.model <- rpart(Class ~ ., data = df, method = "class", minbucket = 20)
pred.tree.model<-predict(tree.model,df,type = "class")
t <- table(pred.tree.model,df$Class)
confusionMatrix(t)


#makek a tree again but just with the training data
tree.train <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
pred.tree.train<-predict(tree.train,train,type = "class")
t <- table(pred.tree.train,train$Class)
confusionMatrix(train$Class,pred.tree.train)
#see how accuracy compares with test set
pred.tree.test<-predict(tree.train,test,type="class")
confusionMatrix(test$Class,pred.tree.test)



#make a random forest model
set.seed(1234)
random.forest <- randomForest(Class ~ ., data = df,ntree = 500, nodesize = 20)
rf.predict <- predict(random.forest, df)
confusionMatrix(df$Class, rf.predict)


#make a random forest just on  training data
set.seed(1234)
random.forest <- randomForest(Class ~ ., data = train,ntree = 500, nodesize = 20)
rf.predict.train <- predict(random.forest, train)
confusionMatrix(train$Class, rf.predict.train)
#see how this model does with the test set.
rf.predict.test <- predict(random.forest, test)
confusionMatrix(test$Class, rf.predict.test)



#maek a bigger tree
#random forest just training
set.seed(1234)
random.forest.1000 <- randomForest(Class ~ ., data = train,ntree = 1000, nodesize = 20)
rf.predict.train.1000 <- predict(random.forest.1000, train)
confusionMatrix(train$Class, rf.predict.train.1000 )

#find the mean decrease gini associated with each varaible
ginis<-as.data.frame(importance(random.forest.1000))
ginis<-arrange(ginis,MeanDecreaseGini)
ginis

#####
# Make ROC
score<-prediction(predict(random.forest.1000, type="prob")[,2],train$Class)
plot(performance(score,"tpr","fpr"),col="blue",main= "Random Forest ROC")
abline(0,1,lty=2)

