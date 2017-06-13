## Case Study 2-- CSC 529---- slope recoded
#Sarah Cummings

library(e1071)
library(caret)
library(ROCR)
library(pROC)

#read in heart disease data from the wen
df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

##Preprocessing
#give appropriate column names
names(df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

#create a binary "heart disease or not" variable
df$DiagnosisBinary<-factor(ifelse(df$num==0,0,1))
head(df)

#remove the original numeric dianosis so we can focus on the binary
vars<-c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
         "thalach","exang", "oldpeak","slope", "ca", "thal", "DiagnosisBinary")
df2<-df[,vars]

#ensure that our binary varaibles are considered categorical
df2$sex<-as.factor(df2$sex)
df2$cp<-as.factor(df2$cp)
df2$fbs<-as.factor(df2$fbs)
df2$restecg<-as.factor(df2$restecg)
df2$exang<-as.factor(df2$exang)
df2$thal<-as.factor(df2$thal)
df2$slope<-as.factor(df2$slope)

##Data Analysis
#categorical varaibles
table(df$num)
table(df2$sex)
table(df2$cp)
table(df2$fbs)
table(df2$restecg)
table(df2$exang)
table(df2$slope)
table(df2$thal)

#quantitative variables
summary(df2$age)
summary(df2$trestbps)
summary(df2$chol)
summary(df2$thalach)
summary(df2$oldpeak)


##Make some initial Visualization
#Age  analysis

box<-ggplot(df2,aes(factor(DiagnosisBinary),age))
box+geom_boxplot()+xlab("Heart Disease Diagnosis")+ggtitle("Difference in Age Among Diagnosis Groups")
#Gender Analysis
bar<-ggplot(df2,aes(sex,fill=DiagnosisBinary))
bar+geom_bar(position = "fill")+ylab("Percentage")+ggtitle("Difference in Diagnosis by Gender")
#blood sugar
fbs<-ggplot(df2,aes(fbs,fill=DiagnosisBinary))
fbs+geom_bar(position = "fill")+ylab("Percentage")+ggtitle("Difference in  Fasting Blood Sugar")
#exerceis angina
exang<-ggplot(df2,aes(exang,fill=DiagnosisBinary))
exang+geom_bar(position = "fill")+ylab("Percentage")+ggtitle("Difference in Exercise Angina")
#resting blood pressure
rbp<-ggplot(df2,aes(factor(DiagnosisBinary),trestbps))
rbp+geom_boxplot()+xlab("Heart Disease Diagnosis")+ggtitle("Difference in Blood Pressure Among Diagnoses")
#chlor
chlor<-ggplot(df2,aes(factor(DiagnosisBinary),chol))
chlor+geom_boxplot()+xlab("Heart Disease Diagnosis")+ggtitle("Difference in Cholestorol Among Diagnoses")
#heart rate
hr<-ggplot(df2,aes(factor(DiagnosisBinary),thalach))
hr+geom_boxplot()+xlab("Heart Disease Diagnosis")+ggtitle("Difference in Max Heart Rate Among Diagnoses")

#are our numeric varaibles correlated?
cor.test(df$age,df$chol)
#pull just numeric variables from the data
numerics<-df2[,unlist(lapply(df2, is.numeric))]

#remove ca since it is ordinal
colnames(numerics[1:5])

library(corrplot)
#assess the correlation of the numerical varaibles
M<-cor(numerics[1:5])
corrplot(M,main="Correlations Among Numeric Varaibles")
pairs(df2[,unlist(lapply(df2, is.numeric))])


##Begin the modeling process

#create a trainging and testing split
set.seed(1234)
ind <- sample(2, nrow(df2), replace=TRUE, prob=c(0.8, 0.2))
train <- df2[ind==1,]
test <- df2[ind==2,]

#make a svm: radial
svm_model <- svm(DiagnosisBinary ~ ., data=train)
summary(svm_model)
pred <- predict(svm_model,test)
t <- table(pred,test$DiagnosisBinary)
confusionMatrix(t) #Accuracy is 0.8302  

#new linear svm
svm_model_lin <- svm(DiagnosisBinary ~ ., data=train,kernel="linear")
summary(svm_model_lin)
pred <- predict(svm_model_lin,test)
t <- table(pred,test$DiagnosisBinary)
confusionMatrix(t) # Accuracy is  0.8868

#new polynomial svm
svm_model_poly <- svm(DiagnosisBinary ~ ., data=train,kernel="polynomial")
summary(svm_model_poly)
pred <- predict(svm_model_poly,test)
t <- table(pred,test$DiagnosisBinary)
confusionMatrix(t) #Accuracy is 0.7736-- terrible specificity

#new sigmoid svm
svm_model_sigmoid <- svm(DiagnosisBinary ~ ., data=train,kernel="sigmoid")
summary(svm_model_sigmoid)
pred <- predict(svm_model_sigmoid,test)
t <- table(pred,test$DiagnosisBinary)
confusionMatrix(t,positive='1') #Accuracy is 0.8679 


#check to see if this data could be modeled by a simple logistic regression
mylogit <- glm(DiagnosisBinary~., data = train, family = "binomial")
predict <- predict(mylogit,test, type = 'response')
predict[1:5]
t<-table(test$DiagnosisBinary, predict>0.5)
confusionMatrix(t)
coeftest(mylogit)

#make ROC of final model
# Make ROC

svmPrediction<-predict(svm_model_lin, test,type="prob")
p<-as.data.frame(predict(svm_model_lin, test,type="prob"))
p2<-as.data.frame(p$`predict(svm_model_lin, test, type = "prob")`)
names(p2)<-"pred"
p2$pred<-as.numeric(p2$pred)
true<-as.data.frame(test$DiagnosisBinary)
names(true)<-"true"
true$true<-as.numeric(true$true)
score<-prediction(p2,true)
plot(performance(score,"tpr","fpr"),col="blue",main= "SVM ROC")
abline(0,1,lty=2)

roc(as.numeric(test$DiagnosisBinary),as.numeric(as.matrix((svmPrediction))))


svm_model_lin$decision.values


##SVM tuning
mytunedsvm <- tune.svm(DiagnosisBinary ~ ., kernel = "radial",data=na.omit(train),coef0 = (-1:4), degree = (1:4))
summary(mytunedsvm)
mytunedsvm

svm_tuned<-svm(DiagnosisBinary ~ ., data=train,kernel="radial",degree=1,coef0=-1)
pred <- predict(svm_tuned,test)
t <- table(pred,test$DiagnosisBinary)
confusionMatrix(t,positive='1') #Accuracy is 0.8679 


