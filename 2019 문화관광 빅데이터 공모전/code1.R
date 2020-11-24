setwd("C:/Users/Yoonhee.YOONHEE/Desktop/공모전/문화관광/데이터")
data <- read.csv("data.csv", header = T)
names(data) <- c("id","area","areasize1","gender1","age1","family1","marriage1","income1",
                 "q36","q37","q38b1","q43","q45a1",
                 "q5","q7a1","q7a2","q9","q13a1a1","q13a4a1","q13a1a2","q13a4a2",
                 "q14","q14a1","q22","q23","q30","q31","q32","q33")

table(data$Q38B1)
table(data$GENDER)
data<-data[,-11]

#카이제곱 검정
result<-read.csv("result.csv")
head(result)
result<-result[,-1]
install.packages("ca")
library(ca)
age1xarea <- table(data$age1, data$area)
age1xarea
age <- table(data$gender1, data$age1)
areaxgender1 <- table(data$area, data$gender1)
ca_age1xarea <- ca(age1xarea)
plot(ca_age1xarea)
ca_age <- ca(age)
plot(ca_age)
age1xgender
age1xarea
age

#MCA
cluster_data<-data[,c(2,4,5)]
library("FactoMineR")
library("factoextra")
head(cluster_data)
cluster_data$SQ1<-as.character(cluster_data$SQ1)
cluster_data$GENDER<-as.character(cluster_data$GENDER)
cluster_data$AGE_G1<-as.character(cluster_data$AGE_G1)
summary(cluster_data)
mca1=MCA(cluster_data,graph=TRUE)
mca1

cats = apply(cluster_data, 2, function(x) nlevels(as.factor(x)))
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
mca1_obs_df = data.frame(mca1$ind$coord)
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")


#random forest
install.packages("randomForest")
library(randomForest)
data <-read.csv('data.csv')
result<-read.csv("result.csv")

data$SQ1 <- as.character(data$SQ1)
data$GUMO <- as.character(data$GUMO)
data$GENDER <- as.character(data$GENDER)
data$AGE_G1 <- as.character(data$AGE_G1)
data$FAM <- as.character(data$FAM)
data$MER <- as.character(data$MER)
data$INC <- as.character(data$INC)
data$Q37 <- as.character(data$Q37)
data$Q38B1 <- as.character(data$Q38B1)
data$Q45A1 <- as.character(data$Q45A1)
data$Q5 <- as.character(data$Q5)
data$Q14 <- as.character(data$Q14)
data$Q22 <- as.character(data$Q22)
data$Q23 <- as.character(data$Q23)
data$Q30 <- as.character(data$Q30)
data$Q31 <- as.character(data$Q31)
data$Q32 <- as.character(data$Q32)
data$Q33 <- as.character(data$Q33)

new_data<-data
new_data$Q43[is.na(new_data$Q43)]<-mean(new_data$Q43, na.rm=T)
new_data$Q7A1[is.na(new_data$Q7A1)]<-mean(new_data$Q7A1, na.rm=T)
new_data$Q7A2[is.na(new_data$Q7A2)]<-mean(new_data$Q7A2, na.rm=T)
new_data$Q14A1[is.na(new_data$Q14A1)]<-mean(new_data$Q14A1, na.rm=T)
point <- new_data$Q9/((new_data$Q13A1A1*5+new_data$Q13A1A2*2)*4)

data1 <- cbind(new_data,point)

#data_yb random forest
data_yb <- data1[data1$AGE_G1==1|data1$AGE_G1==2,]
head(data_yb)
data_yb<-data_yb[,-1:-7]
data_yb<-data_yb[,-c(10,11,13)]
data_yb <- data_yb[,-c(10,11)]
head(data_yb)

model_yb <- randomForest(point~., data=data_yb, importance=T, na.action = na.omit)
importance(model_yb)
varImpPlot(model_yb)

#data_mb random forest
data_mb <- data1[data1$AGE_G1==3|data1$AGE_G1==4|data1$AGE_G1==5|data1$AGE_G1==6,]
head(data_mb)
data_mb<-data_mb[,-1:-7]
data_mb<-data_mb[,-c(10,11,13)]
data_mb <- data_mb[,-c(10,11)]
head(data_mb)

model_mb <- randomForest(point~., data=data_mb, importance=T, na.action = na.omit)
importance(model_mb)
varImpPlot(model_mb)

#data_ob random forest
data_ob <- data1[data1$AGE_G1==7,]
head(data_ob)
data_mb<-data_ob[,-1:-7]
data_ob<-data_ob[,-c(10,11,13)]
data_ob <- data_ob[,-c(10,11)]
head(data_ob)

model_ob <- randomForest(point~., data=data_ob, importance=T, na.action = na.omit)
importance(model_ob)
varImpPlot(model_ob)

#data_yb decision tree
install.packages("caret")
library(caret)
set.seed(1234)

sample_numyb <-  sample(1:nrow(data_yb), size = round(0.7 * nrow(data_yb)))
train_data_yb <- data_yb[sample_numyb, ]
test_data_yb <- data_yb[-sample_numyb,]

library(rpart)
library(party)

train_data_yb$INC <- as.factor(train_data_yb$INC)
train_data_yb$Q37 <- as.factor(train_data_yb$Q37)
train_data_yb$Q43 <- as.factor(train_data_yb$Q43)
train_data_yb$Q5 <- as.factor(train_data_yb$Q5)
train_data_yb$Q14 <- as.factor(train_data_yb$Q14)
train_data_yb$Q22 <- as.factor(train_data_yb$Q22)
train_data_yb$Q23 <- as.factor(train_data_yb$Q23)
train_data_yb$Q30 <- as.factor(train_data_yb$Q30)
train_data_yb$Q31 <- as.factor(train_data_yb$Q32)
train_data_yb$Q33 <- as.factor(train_data_yb$Q33)
train_data_yb$Q45A1 <- as.factor(train_data_yb$Q45A1)
train_data_yb$Q32 <- as.factor(train_data_yb$Q32)
summary(train_data_yb)

tree_yb <- ctree(point~INC+Q7A1+Q43+Q45A1 ,data=train_data_yb)
plot(tree_yb)

#data_yb error rate
test_data_yb$INC <- as.factor(test_data_yb$INC)
test_data_yb$Q37 <- as.factor(test_data_yb$Q37)
test_data_yb$Q43 <- as.factor(test_data_yb$Q43)
test_data_yb$Q5 <- as.factor(test_data_yb$Q5)
test_data_yb$Q14 <- as.factor(test_data_yb$Q14)
test_data_yb$Q22 <- as.factor(test_data_yb$Q22)
test_data_yb$Q23 <- as.factor(test_data_yb$Q23)
test_data_yb$Q30 <- as.factor(test_data_yb$Q30)
test_data_yb$Q31 <- as.factor(test_data_yb$Q32)
test_data_yb$Q33 <- as.factor(test_data_yb$Q33)
test_data_yb$Q45A1 <- as.factor(test_data_yb$Q45A1)
test_data_yb$Q32 <- as.factor(test_data_yb$Q32)

treepred <- predict(tree_yb, test_data_yb)
table(treepred, test_data_yb$point)

levels(train_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q32) <- c(1,2,3,4,5,6,7)

summary(train_data_yb)
summary(test_data_yb)
levels(train_data_yb$Q33)
levels(test_data_yb$Q33) <- c(1,2,3,4,5,6,7,8,9,10)
levels(test_data_yb$Q43)<- levels(train_data_yb$Q43)
levels(test_data_yb$Q14)<- levels(train_data_yb$Q14)
levels(test_data_yb$Q37)<- levels(train_data_yb$Q37)

library(rpart)
tree_yb_error <- rpart(point~INC+Q7A1+Q43+Q45A1,data=train_data_yb)
printcp(tree_yb_error)

#data_mb decision tree
library(caret)
set.seed(1234)

sample_nummb <-  sample(1:nrow(data_mb), size = round(0.7 * nrow(data_mb)))
train_data_mb <- data_mb[sample_nummb, ]
test_data_mb <- data_yb[-sample_nummb,]

library(rpart)
library(party)

train_data_mb$INC <- as.factor(train_data_mb$INC)
train_data_mb$Q37 <- as.factor(train_data_mb$Q37)
train_data_mb$Q43 <- as.factor(train_data_mb$Q43)
train_data_mb$Q5 <- as.factor(train_data_mb$Q5)
train_data_mb$Q14 <- as.factor(train_data_mb$Q14)
train_data_mb$Q22 <- as.factor(train_data_mb$Q22)
train_data_mb$Q23 <- as.factor(train_data_mb$Q23)
train_data_mb$Q30 <- as.factor(train_data_mb$Q30)
train_data_mb$Q31 <- as.factor(train_data_mb$Q32)
train_data_mb$Q33 <- as.factor(train_data_mb$Q33)
train_data_mb$Q45A1 <- as.factor(train_data_mb$Q45A1)
train_data_mb$Q32 <- as.factor(train_data_mb$Q32)
summary(train_data_mb)

tree_mb <- ctree(point~INC+Q7A1+Q43+Q45A1 ,data=train_data_mb)
plot(tree_mb)

#data_mb error rate
test_data_mb$INC <- as.factor(test_data_mb$INC)
test_data_mb$Q37 <- as.factor(test_data_mb$Q37)
test_data_mb$Q43 <- as.factor(test_data_mb$Q43)
test_data_mb$Q5 <- as.factor(test_data_mb$Q5)
test_data_mb$Q14 <- as.factor(test_data_mb$Q14)
test_data_mb$Q22 <- as.factor(test_data_mb$Q22)
test_data_mb$Q23 <- as.factor(test_data_mb$Q23)
test_data_mb$Q30 <- as.factor(test_data_mb$Q30)
test_data_mb$Q31 <- as.factor(test_data_mb$Q32)
test_data_mb$Q33 <- as.factor(test_data_mb$Q33)
test_data_mb$Q45A1 <- as.factor(test_data_mb$Q45A1)
test_data_mb$Q32 <- as.factor(test_data_mb$Q32)

treepred_mb <- predict(tree_mb, test_data_mb)
table(treepred_mb, test_data_mb$point)

levels(train_data_mb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(test_data_mb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(train_data_mb$Q31) <- c(1,2,3,4,5,6,7)
levels(test_data_mb$Q31) <- c(1,2,3,4,5,6,7)
levels(train_data_mb$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_mb$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_mb$Q33) <- c(1,2,3,4,5,6,7,8,9,10)
levels(test_data_mb$Q43)<- levels(train_data_mb$Q43)
levels(test_data_mb$Q14)<- levels(train_data_mb$Q14)
levels(test_data_mb$Q37)<- levels(train_data_mb$Q37)

library(rpart)
tree_mb_error <- rpart(point~INC+Q7A1+Q43+Q45A1,data=train_data_mb)
printcp(tree_mb_error)

#data_ob decision tree
library(caret)
set.seed(1234)

sample_numob <-  sample(1:nrow(data_ob), size = round(0.7 * nrow(data_ob)))
train_data_ob <- data_ob[sample_numob, ]
test_data_ob <- data_ob[-sample_numob,]

library(rpart)
library(party)

train_data_ob$INC <- as.factor(train_data_ob$INC)
train_data_ob$Q37 <- as.factor(train_data_ob$Q37)
train_data_ob$Q43 <- as.factor(train_data_ob$Q43)
train_data_ob$Q5 <- as.factor(train_data_ob$Q5)
train_data_ob$Q14 <- as.factor(train_data_ob$Q14)
train_data_ob$Q22 <- as.factor(train_data_ob$Q22)
train_data_ob$Q23 <- as.factor(train_data_ob$Q23)
train_data_ob$Q30 <- as.factor(train_data_ob$Q30)
train_data_ob$Q31 <- as.factor(train_data_ob$Q32)
train_data_ob$Q33 <- as.factor(train_data_ob$Q33)
train_data_ob$Q45A1 <- as.factor(train_data_ob$Q45A1)
train_data_ob$Q32 <- as.factor(train_data_ob$Q32)
summary(train_data_ob)

tree_ob <- ctree(point~INC+Q7A1+Q43+Q45A1 ,data=train_data_ob)
plot(tree_ob)


#data_ob error rate
test_data_ob$INC <- as.factor(test_data_ob$INC)
test_data_ob$Q37 <- as.factor(test_data_ob$Q37)
test_data_ob$Q43 <- as.factor(test_data_ob$Q43)
test_data_ob$Q5 <- as.factor(test_data_ob$Q5)
test_data_ob$Q14 <- as.factor(test_data_ob$Q14)
test_data_ob$Q22 <- as.factor(test_data_ob$Q22)
test_data_ob$Q23 <- as.factor(test_data_ob$Q23)
test_data_ob$Q30 <- as.factor(test_data_ob$Q30)
test_data_ob$Q31 <- as.factor(test_data_ob$Q32)
test_data_ob$Q33 <- as.factor(test_data_ob$Q33)
test_data_ob$Q45A1 <- as.factor(test_data_ob$Q45A1)
test_data_ob$Q32 <- as.factor(test_data_ob$Q32)

treepred_ob <- predict(tree_ob, test_data_ob)
table(treepred_ob, test_data_ob$point)

levels(train_data_ob$Q45A1) <- c(1,2,3,4,5,6,7)
levels(test_data_ob$Q45A1) <- c(1,2,3,4,5,6,7)
levels(train_data_ob$Q31) <- c(1,2,3,4,5,6,7)
levels(test_data_ob$Q31) <- c(1,2,3,4,5,6,7)
levels(train_data_ob$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_ob$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_ob$Q33) <- c(1,2,3,4,5,6,7,8,9,10)
levels(test_data_ob$Q43)<- levels(train_data_ob$Q43)
levels(test_data_ob$Q14)<- levels(train_data_ob$Q14)
levels(test_data_ob$Q37)<- levels(train_data_ob$Q37)

library(rpart)
tree_ob_error <- rpart(point~INC+Q7A1+Q43+Q45A1,data=train_data_ob)
printcp(tree_ob_error)





























#data_yb error rate
test_data_yb$INC <- as.factor(test_data_yb$INC)
test_data_yb$Q37 <- as.factor(test_data_yb$Q37)
test_data_yb$Q43 <- as.factor(test_data_yb$Q43)
test_data_yb$Q5 <- as.factor(test_data_yb$Q5)
test_data_yb$Q14 <- as.factor(test_data_yb$Q14)
test_data_yb$Q22 <- as.factor(test_data_yb$Q22)
test_data_yb$Q23 <- as.factor(test_data_yb$Q23)
test_data_yb$Q30 <- as.factor(test_data_yb$Q30)
test_data_yb$Q31 <- as.factor(test_data_yb$Q32)
test_data_yb$Q33 <- as.factor(test_data_yb$Q33)
test_data_yb$Q45A1 <- as.factor(test_data_yb$Q45A1)
test_data_yb$Q32 <- as.factor(test_data_yb$Q32)

treepred <- predict(tree_yb, test_data_yb)
table(treepred, test_data_yb$point)

levels(train_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q45A1) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q31) <- c(1,2,3,4,5,6,7)
levels(train_data_yb$Q32) <- c(1,2,3,4,5,6,7)
levels(test_data_yb$Q32) <- c(1,2,3,4,5,6,7)

summary(train_data_yb)
summary(test_data_yb)
levels(train_data_yb$Q33)
levels(test_data_yb$Q33) <- c(1,2,3,4,5,6,7,8,9,10)
levels(test_data_yb$Q43)<- levels(train_data_yb$Q43)
levels(test_data_yb$Q14)<- levels(train_data_yb$Q14)
levels(test_data_yb$Q37)<- levels(train_data_yb$Q37)

library(rpart)
tree_yb_error <- rpart(point~INC+Q7A1+Q43+Q45A1,data=train_data_yb)
printcp(tree_yb_error)

data <- as.data.frame(data)
#################





