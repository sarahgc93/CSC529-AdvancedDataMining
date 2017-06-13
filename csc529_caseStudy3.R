#case study 3
#sarah cummings


library(ggplot2)
library(dplyr)
library(bnlearn)
library(igraph)

#setwd('C:\\Users\\scummings\\Desktop\\MyFiles')
setwd("/Users/sarahcummings/Documents/csc529")

#read in theh dataset
df<-read.csv("asia10K.csv")

#find the distribution of the columns
table(df$Smoker)
table(df$LungCancer)
table(df$VisitToAsia)
table(df$Tuberculosis)
table(df$TuberculosisOrCancer)
table(df$X.ray)
table(df$Bronchitis)
table(df$Dyspnea)

#make some simple graphs showing their distributions
g<-ggplot(df,aes(factor(LungCancer),fill=factor(Smoker)))+geom_bar()+ggtitle("Lung Cancer Proportions Among Smokers and Non-Smokers")
g
g2<-ggplot(df,aes(factor(Tuberculosis),fill=factor(VisitToAsia)))+geom_bar()+ggtitle("Lung Cancer Proportions Among Smokers and Non-Smokers")
g2
#since to yes is so small, lets filter out the no to see more closely
d2<-filter(df,Tuberculosis=="yes")
g2.0<-ggplot(d2,aes(factor(Tuberculosis),fill=factor(VisitToAsia)))+geom_bar()+ggtitle("Asian Travel Among Those With TB")
g2.0

d3<-filter(df,Tuberculosis=="no")
g3.0<-ggplot(d3,aes(factor(Tuberculosis),fill=factor(VisitToAsia)))+geom_bar()+ggtitle("Asian Travel Among Those Without TB")
g3.0

g2<-ggplot(df,aes(factor(Smoker),fill=factor(LungCancer)))+geom_bar()+ggtitle("Lung Cancer Proportions Among Smokers and Non-Smokers")
g2

d2<-filter(df,VisitToAsia=="yes")
g2.0<-ggplot(d2,aes(factor(VisitToAsia),fill=factor(Tuberculosis)))+geom_bar()+ggtitle("Asian Travel Among Those With TB")
g2.0

d3<-filter(df,VisitToAsia=="no")
g3.0<-ggplot(d3,aes(factor(VisitToAsia),fill=factor(Tuberculosis)))+geom_bar()+ggtitle("Asian Travel Among Those Without TB")
g3.0

g4<-ggplot(df,aes(factor(TuberculosisOrCancer),fill=factor(Dyspnea)))+geom_bar()
g4+ ggtitle("Does Dysphnea Mean Tb or Cancer?")

str(df)

##make a gs model
e = empty.graph(colnames(df))
plot(e)
bn.gs <- gs(df)
score(bn.gs,df)

plot(bn.gs)
#since the colnames don't fit in the nodes, I will rename them more simply
colnames(df)
names(df)<-c("S","L","A","T","E","X","B","D")
#reorder columns for better graph
keep<-c("A", "S", "T", "L", "B", "E", "X", "D")
df<-df[,keep]

#create a random/ generated network
net <- model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E]")
net
plot(net)
score(net,df)

#make a grow shrink BN
bn.gs <- gs(df)
bn.gs
plot(bn.gs)
score(bn.gs,df)

#make other constraint based algoritms
bn2 <- iamb(df)
bn2
plot(bn2)

bn3 <- fast.iamb(df)
bn3
plot(bn3)

bn4 <- inter.iamb(df)
bn4

#make a hillclimbing algoritm  model
bn.hc <- hc(df, score = "aic")
bn.hc
plot(bn.hc)
bn.hcFit<-bn.fit(hc(df),df)
bn.hcFit
score(bn.hc,df)

#find the probabilities of the diseases
cpquery(bn.hcFit, (T=="yes"), TRUE)
cpquery(bn.hcFit, (L=="yes"), TRUE)
cpquery(bn.hcFit, (B=="yes"), TRUE)

#find the probility of the diseases given the patient has been to asia and isn't a smoker
cpquery(bn.hcFit, (T=="yes"), (A=="yes" & S=="no"))
cpquery(bn.hcFit, (L=="yes"), (A=="yes" & S=="no"))
cpquery(bn.hcFit, (B=="yes"), (A=="yes" & S=="no"))



