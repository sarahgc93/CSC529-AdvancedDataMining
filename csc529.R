##T1

setwd('C:\\Users\\scummings\\Desktop\\MyFiles')
df<-read.csv("Video_Store_DLQuiz.csv")
head(df)

mean(df$Rentals)
sd(df$Rentals)
summary(df$Age)
hist(df$Rentals, breaks = 10)
boxplot(df$Rentals ~ df$Gender)


#zscore norm of age
mu <- mean(df$Age)
sd <- sd(df$Age)
Age.Zscore <- (df$Age - mu)/sd
Age.Zscore

#plot
plot(df$Age,df$Income)

#corr between age and rentals
cor(df$Age,df$Rentals)

#use table to compare Gender and Incidentals
table(df$Gender,df$Incidentals)

#discretize the age var
df$Age.cut <- cut(df$Age,5)
table(df$Age.cut)

#create dummy variables
df$Genre.Action <- (df$Genre == "Action") * 1
head(df)

#create df of good customers (more than 30 rentals)
goodCustomers <- df[ which(df$Rentals > 30), ]



##T2
#install.packages("ISLR")
library(ISLR)
attach(Carseats)
data <- Carseats
help(Carseats)

df<-Carseats
mean(df$Income)

#create a new variable called "StrongSales":
data$StrongSales <- as.factor( ifelse(data$Sales >=10, "Yes", "No") )

#discard the original column:
data$Sales <- NULL

summary(data)

#create a simple 80-20 split.

set.seed(1234)





ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]
length(test$Income)


#Decision Trees
library("rpart")
model.tree <- rpart(StrongSales ~ . , data=train)
summary(model.tree)
plot(model.tree)
text(model.tree, pretty=0)


#We can then use the model to make predictions:
pred.train.tree <- predict(model.tree, train, type = "class")

#We can then compute the misclassification rate of the model:
mean(pred.train.tree != train$StrongSales)

pred.test.tree <- predict(model.tree, test, type = "class")
mean(pred.test.tree != test$StrongSales)

#confusion Matrix
table(pred.test.tree,test$StrongSales)

##Naive Bayes
library("e1071")
model.nb <- naiveBayes(StrongSales ~ . , data=train)
mean(model.nb !=train$StrongSales)

pred<-predict(model.nb,train,type = "class")
#calculate misclassifcation
mean(pred != train$StrongSales)

pred2<-predict(model.nb,test,type = "class")
mean(pred2 != test$StrongSales)
table(pred2,test$StrongSales)


##KNN

library('class')
set.seed(1234)
pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 3)
mean(pred.test.knn != test$StrongSales) 

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 5)
mean(pred.test.knn != test$StrongSales)

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 10)
mean(pred.test.knn != test$StrongSales)
table(pred.test.knn,test$StrongSales)

###POST IN FORUM
#Compare the misclassifaction rates of the training and testing data 
#for the decision tree and naïve Bayes models.  What can you conclude?

#Give the conditional probability table of Education from the naïve Bayes mode.  
#Explain how the algorithm deals with continuous variables.

$tables$Education
Education
Y        [,1]     [,2]
No  14.0613 2.560814
Yes 13.8125 2.872281

#Normalize the numerical data using z-scores and replace the categorical variables with dummy variables. 
#Rerun knn and report your results.

head(df)

#zscore norm of sales
mu2 <- mean(df$Age)
sd2 <- sd(df$Age)
Sales.Zscore <- (df$Age - mu)/sd


